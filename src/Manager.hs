{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Manager where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import Data.Default
import Data.Yaml
import qualified Database.Persist.Sql as Sql
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Web.Scotty.Trans as Scotty
import System.FilePath
import System.FilePath.Glob

import CommonTypes
import Types
import Config
import Database
import Schedule
import Logging

application :: ScottyT Error ConnectionM ()
application = do
  Scotty.defaultHandler raiseError

  Scotty.get "/stats" getStatsA
  Scotty.get "/stats/:name" getQueueStatsA

  Scotty.get "/queue" getQueuesA
  Scotty.get "/queue/:name" getQueueA
  Scotty.post "/queue/:name" updateQueueA
  Scotty.put "/queue" addQueueA

  Scotty.put "/queue/:name" enqueueA
  Scotty.delete "/queue/:name/:seq" removeJobA
  Scotty.delete "/queue/:name" removeQueueA

  Scotty.get "/job/:id" getJobA
  Scotty.delete "/job/:id" removeJobByIdA
  Scotty.get "/jobs" getJobsA

  Scotty.get "/schedule" getSchedulesA
  Scotty.put "/schedule" addScheduleA

  Scotty.get "/type" getJobTypesA
  Scotty.get "/type/:name" getJobTypeA

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

-- | Get URL parameter in form ?name=value
getUrlParam :: B.ByteString -> Action (Maybe B.ByteString)
getUrlParam key = do
  rq <- Scotty.request
  let qry = Wai.queryString rq
  return $ join $ lookup key qry

raise404 :: String -> Action ()
raise404 t = do
  Scotty.status status404
  Scotty.text $ TL.pack $ "Specified " ++ t ++ " not found."

raiseError :: Error -> Action ()
raiseError QueueNotExists = raise404 "queue"
raiseError JobNotExists   = raise404 "job"
raiseError FileNotExists  = raise404 "file"
raiseError QueueNotEmpty  = Scotty.status status403
raiseError e = raise e

getQueuesA :: Action ()
getQueuesA = do
  qes <- runDBA getAllQueues'
  -- let qnames = map (queueName . entityVal) qes
  Scotty.json qes

parseStatus' :: Maybe JobStatus -> Maybe B.ByteString -> Action (Maybe JobStatus)
parseStatus' dflt str = parseStatus dflt (raise InvalidJobStatus) str

getQueueA :: Action ()
getQueueA = do
  qname <- Scotty.param "name"
  st <- getUrlParam "status"
  fltr <- parseStatus' (Just New) st
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

removeJobByIdA :: Action ()
removeJobByIdA = do
  jid <- Scotty.param "id"
  runDBA $ removeJobById jid
  Scotty.json ("done" :: String)

removeQueueA :: Action ()
removeQueueA = do
  qname <- Scotty.param "name"
  forced <- getUrlParam "forced"
  st <- getUrlParam "status"
  fltr <- parseStatus' Nothing st
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
  fltr <- parseStatus' (Just New) st
  jobs <- runDBA $ loadJobsByStatus fltr
  Scotty.json jobs

deleteJobsA :: Action ()
deleteJobsA = do
  name <- Scotty.param "name"
  st <- getUrlParam "status"
  fltr <- parseStatus' Nothing st
  case fltr of
    Nothing -> raise InvalidJobStatus
    Just status -> runDBA $ removeJobs name status

getJobTypesA :: Action ()
getJobTypesA = do
  dirs <- liftIO $ getConfigDirs "jobtypes"
  files <- forM dirs $ \dir -> liftIO $ glob (dir </> "*.yaml")
  ts <- forM (concat files) $ \path -> do
             r <- liftIO $ decodeFileEither path
             case r of
               Left err -> do
                  lift $ reportError $ show err
                  return []
               Right jt -> return [jt]
  let types = concat ts :: [JobType]
  Scotty.json types

getJobTypeA :: Action ()
getJobTypeA = do
  name <- Scotty.param "name"
  r <- liftIO $ loadTemplate name
  case r of
    Left err -> raise err
    Right jt -> Scotty.json jt

