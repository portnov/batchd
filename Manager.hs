{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Manager where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Data.ByteString as B
import qualified Data.Map as M
-- import Data.Aeson
import Data.Default
import Database.Persist
import qualified Database.Persist.Sql as Sql
import qualified Database.Persist.Sqlite as Sqlite
import System.Environment
import Text.Printf
import Network.HTTP.Types
import qualified Network.Wai as Wai
-- import Web.Scotty
import Web.Scotty.Trans as Scotty

import CommonTypes
import Types
import Database
import Schedule

runApplication :: Sql.ConnectionPool -> IO ()
runApplication pool = do
  let options = def
  let r m = runReaderT (runConnection m) pool
  scottyOptsT options r application

application :: ScottyT Error ConnectionM ()
application = do
  -- runDBA (Sql.runMigration migrateAll)
  Scotty.get "/queues" getQueuesA
  Scotty.get "/queue/:name" getQueueA
  Scotty.post "/queue/:name" updateQueueA
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
  pool <- getPool
  Sql.runSqlPool (Sql.runMigration migrateAll) pool
  runApplication pool

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

