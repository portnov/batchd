{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Manager where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.Default
import qualified Database.Persist.Sql as Sql
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Web.Scotty.Trans as Scotty

import CommonTypes
import Types
import Config
import Database
import Schedule

application :: ScottyT Error ConnectionM ()
application = do
  -- runDBA (Sql.runMigration migrateAll)
  Scotty.get "/stats" getStatsA
  Scotty.get "/queues" getQueuesA
  Scotty.get "/queue/:name" getQueueA
  Scotty.post "/queue/:name" updateQueueA
  Scotty.get "/queue/:name/stats" getQueueStatsA
  Scotty.put "/queues" addQueueA

  Scotty.put "/queue/:name" enqueueA
  Scotty.delete "/queue/:name/:seq" removeJobA
  Scotty.delete "/queue/:name" removeQueueA

  Scotty.get "/job/:id" getJobA
  Scotty.get "/jobs" getJobsA

  Scotty.get "/schedules" getSchedulesA
  Scotty.put "/schedules" addScheduleA

runManager :: IO ()
runManager = do
  cfgR <- loadGlobalConfig
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      pool <- getPool cfg
      let connInfo = ConnectionInfo cfg pool
      Sql.runSqlPool (Sql.runMigration migrateAll) (ciPool connInfo)
      runApplication connInfo

runApplication :: ConnectionInfo -> IO ()
runApplication connInfo = do
  let options = def
  let r m = runReaderT (runConnection m) connInfo
  scottyOptsT options r application

getUrlParam :: B.ByteString -> Action (Maybe B.ByteString)
getUrlParam key = do
  rq <- Scotty.request
  let qry = Wai.queryString rq
  return $ join $ lookup key qry

getQueuesA :: Action ()
getQueuesA = do
  qes <- runDBA getAllQueues'
  -- let qnames = map (queueName . entityVal) qes
  Scotty.json qes

parseStatus :: Maybe JobStatus -> Maybe B.ByteString -> Action (Maybe JobStatus)
parseStatus dflt Nothing = return dflt
parseStatus _ (Just "all") = return Nothing
parseStatus _ (Just "new") = return $ Just New
parseStatus _ (Just "processing") = return $ Just Processing
parseStatus _ (Just "done") = return $ Just Done
parseStatus _ (Just "failed") = return $ Just Failed
parseStatus _ (Just _) = raise InvalidJobStatus

getQueueA :: Action ()
getQueueA = do
  qname <- Scotty.param "name"
  st <- getUrlParam "status"
  fltr <- parseStatus (Just New) st
  jobs <- runDBA $ loadJobs qname fltr
  Scotty.json jobs

getQueueStatsA :: Action ()
getQueueStatsA = do
  qname <- Scotty.param "name"
  stats <- runDBA $ getQueueStats qname
  Scotty.json stats

getStatsA :: Action ()
getStatsA = do
  stats <- runDBA getStats
  Scotty.json stats

enqueueA :: Action ()
enqueueA = do
  jinfo <- jsonData
  qname <- Scotty.param "name"
  r <- runDBA $ enqueue qname jinfo
  Scotty.json r

removeJobA :: Action ()
removeJobA = do
  qname <- Scotty.param "name"
  jseq <- Scotty.param "seq"
  runDBA $ removeJob qname jseq
  Scotty.json ("done" :: String)

removeQueueA :: Action ()
removeQueueA = do
  qname <- Scotty.param "name"
  forced <- getUrlParam "forced"
  st <- getUrlParam "status"
  fltr <- parseStatus Nothing st
  case fltr of
    Nothing -> do
      r <- runDBA' $ deleteQueue qname (forced == Just "true")
      case r of
        Left QueueNotEmpty -> do
            Scotty.status status403
        Left e -> Scotty.raise e
        Right _ -> Scotty.json ("done" :: String)
    Just status -> do
        runDBA $ removeJobs qname status
        Scotty.json ("done" :: String)

getSchedulesA :: Action ()
getSchedulesA = do
  ss <- runDBA loadAllSchedules
  Scotty.json ss

addScheduleA :: Action ()
addScheduleA = do
  sd <- jsonData
  name <- runDBA $ addSchedule sd
  Scotty.json name

addQueueA :: Action ()
addQueueA = do
  qd <- jsonData
  name <- runDBA $ addQueue qd
  Scotty.json name

updateQueueA :: Action ()
updateQueueA = do
  name <- Scotty.param "name"
  upd <- jsonData
  runDBA $ updateQueue name upd
  Scotty.json ("done" :: String)

getJobA :: Action ()
getJobA = do
  jid <- Scotty.param "id"
  result <- runDBA $ getJobResult jid
  Scotty.json result

getJobsA :: Action ()
getJobsA = do
  st <- getUrlParam "status"
  fltr <- parseStatus (Just New) st
  jobs <- runDBA $ loadJobsByStatus fltr
  Scotty.json jobs

deleteJobsA :: Action ()
deleteJobsA = do
  name <- Scotty.param "name"
  st <- getUrlParam "status"
  fltr <- parseStatus Nothing st
  case fltr of
    Nothing -> raise InvalidJobStatus
    Just status -> runDBA $ removeJobs name status

