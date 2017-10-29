{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Batchd.Daemon.Database where

import Control.Monad
import Control.Monad.Reader
import qualified Control.Monad.State as State

import Data.Time
import Database.Persist
import Data.Maybe
import Data.Int
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE

import           Database.Persist.Sql as Sql
import           Database.Persist.Sqlite as Sqlite
import           Database.Persist.Postgresql as Postgres
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Daemon.Types
import Batchd.Common.Data
import Batchd.Common.Schedule
import Batchd.Daemon.Schedule

getPool :: GlobalConfig -> LoggingTState -> IO Sql.ConnectionPool
getPool cfg lts =
  case dbcDriver cfg of
    Sqlite -> runLoggingT (Sqlite.createSqlitePool (dbcConnectionString cfg) 1) lts
    PostgreSql -> do
        let str = TE.encodeUtf8 (dbcConnectionString cfg)
        runLoggingT (Postgres.createPostgresqlPool str 10) lts

connectPool :: Daemon Sql.ConnectionPool
connectPool = do
  cfg <- askConfig
  lts <- askLoggingStateM
  pool <- liftIO $ getPool cfg lts
  Daemon $ lift $ State.modify $ \st -> st {ciPool = Just pool}
  return pool

buildJobInfo :: Int64 -> Job -> Maybe JobResult -> JobParamInfo -> JobInfo
buildJobInfo jid j mbr params =
   JobInfo {
      jiId = jid,
      jiQueue = jobQueueName j,
      jiType = jobTypeName j,
      jiCreateTime = jobCreateTime j,
      jiStartTime = jobStartTime j,
      jiSeq = jobSeq j,
      jiUserName = jobUserName j,
      jiStatus = jobStatus j,
      jiTryCount = jobTryCount j,
      jiHostName = jobHostName j,
      jiResultTime = fmap jobResultTime mbr,
      jiExitCode = fmap jobResultExitCode mbr,
      jiStdout = fmap jobResultStdout mbr,
      jiStderr = fmap jobResultStderr mbr,
      jiParams = params
     }

loadJob :: Key Job -> DB JobInfo
loadJob jkey@(JobKey (SqlBackendKey jid)) = do
  mbJob <- get jkey
  case mbJob of
    Nothing -> throwR JobNotExists
    Just j -> do
      mbr <- selectFirst [JobResultJobId ==. jkey] [Desc JobResultTime]
      ps <- selectList [JobParamJobId ==. jkey] []
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ buildJobInfo jid j (fmap entityVal mbr) params

loadJob' :: Int64 -> DB JobInfo
loadJob' jid = loadJob (JobKey (SqlBackendKey jid))

canLock :: DB Bool
canLock = do
  -- Dirty hack: we can't do "for update" in sqlite
  backend <- ask
  return $ connRDBMS backend /= "sqlite"

lockJob :: JobInfo -> DB ()
lockJob ji = do
  ok <- canLock
  when ok $ do
    let qname = jiQueue ji
        seq = jiSeq ji
    E.select $
      E.from $ \job -> do
      E.where_ $ (job ^. JobQueueName `equals` E.val qname) `eand` (job ^. JobSeq `equals` E.val seq)
      E.locking E.ForUpdate
    return ()

lockQueue :: String -> DB ()
lockQueue qname = do
  ok <- canLock
  when ok $ do
    E.select $
      E.from $ \queue -> do
      E.where_ (queue ^. QueueName `equals` E.val qname)
      E.locking E.ForUpdate
    return ()

loadJobSeq :: String -> Int -> DB JobInfo
loadJobSeq qname seq = do
  mbJe <- getBy (UniqJobSeq qname seq)
  case mbJe of
    Nothing -> throwR JobNotExists
    Just je -> do
      let jkey = entityKey je
      let JobKey (SqlBackendKey jid) = jkey
      mbr <- selectFirst [JobResultJobId ==. jkey] [Desc JobResultTime]
      ps <- selectList [JobParamJobId ==. jkey] []
      let j = entityVal je
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ buildJobInfo jid j (fmap entityVal mbr) params

updateJob :: Int64 -> UpdateList Job -> DB ()
updateJob jid (UpdateList updates) = do
  let jkey = JobKey (SqlBackendKey jid)
  update jkey updates

moveJob :: Int64 -> String -> DB ()
moveJob jid qname = do
  let jkey = JobKey (SqlBackendKey jid)
  oldJob <- loadJob' jid
  lockQueue (jiQueue oldJob)
  lockQueue qname
  seq <- getLastJobSeq qname
  update jkey [JobQueueName =. qname, JobSeq =. (seq+1)]

prioritizeJob :: Int64 -> MoveAction -> DB ()
prioritizeJob jid action = do
  let jkey = JobKey (SqlBackendKey jid)
  oldJob <- loadJob' jid
  let qname = jiQueue oldJob
  let oldSeq = jiSeq oldJob
  lockQueue qname
  case action of
    Last -> do
            lastSeq <- getLastJobSeq qname
            let seq' = lastSeq + 1
            update jkey [JobSeq =. seq']
    First -> do
            lastSeq <- getFirstJobSeq qname
            let seq' = lastSeq - 1
            update jkey [JobSeq =. seq']
    _ -> do
         let next = action == Less
         (mbJid, seq') <- getAdjJob next qname oldSeq
         update jkey [JobSeq =. seq']
         case mbJid of
           Nothing -> return ()
           Just jid' -> do
             let jkey' = JobKey (SqlBackendKey jid')
             update jkey' [JobSeq =. oldSeq]
  

setJobStatus :: JobInfo -> JobStatus -> DB ()
setJobStatus ji status = do
  updateWhere [JobQueueName ==. jiQueue ji, JobSeq ==. jiSeq ji] [JobStatus =. status]

increaseTryCount :: JobInfo -> DB Int
increaseTryCount ji = do
  let qname = jiQueue ji
      seq = jiSeq ji
  mbJe <- getBy (UniqJobSeq qname seq)
  case mbJe of
    Nothing -> throwR JobNotExists
    Just je -> do
      let count = jobTryCount (entityVal je)
          count' = count + 1
      updateWhere [JobQueueName ==. qname, JobSeq ==. seq] [JobTryCount =. count']
      return count'

moveToEnd :: JobInfo -> DB ()
moveToEnd ji = do
  let qname = jiQueue ji
      seq = jiSeq ji
  lockQueue qname
  mbJe <- getBy (UniqJobSeq qname seq)
  case mbJe of
    Nothing -> throwR JobNotExists
    Just je -> do
      lastSeq <- getLastJobSeq qname
      let seq' = lastSeq + 1
      updateWhere [JobQueueName ==. qname, JobSeq ==. seq] [JobSeq =. seq', JobStatus =. New]

getAllQueues :: DB [Entity Queue]
getAllQueues = selectList [] []

getEnabledQueues :: DB [Entity Queue]
getEnabledQueues = selectList [QueueEnabled ==. True] []

getAllQueues' :: DB [Queue]
getAllQueues' = map entityVal `fmap` selectList [] []

getAllowedQueues :: String -> Permission -> DB [Queue]
getAllowedQueues name perm = do
  lst <- E.select $ E.distinct $
         E.from $ \(queue `E.InnerJoin` uperm) -> do
           E.on $ (uperm ^. UserPermissionUserName `equals` E.val name)
                      `eand` (uperm ^. UserPermissionPermission `equals` E.val perm)
                      `eand` ((uperm ^. UserPermissionQueueName `equals` E.just (queue ^. QueueName))
                              `eor` (E.isNothing $ uperm ^. UserPermissionQueueName))
           return queue
  return $ map entityVal lst

getAllJobs :: String -> DB [Entity Job]
getAllJobs qid = selectList [JobQueueName ==. qid] [Asc JobSeq]

getJobs :: String -> Maybe JobStatus -> DB [Entity Job]
getJobs qname mbStatus = do
  let filt = case mbStatus of
               Nothing -> []
               Just status -> [JobStatus ==. status]
  selectList ([JobQueueName ==. qname] ++ filt) [Asc JobSeq]

loadJobs :: String -> Maybe JobStatus -> DB [JobInfo]
loadJobs qname mbStatus = do
  mbQ <- getQueue qname
  case mbQ of
    Nothing -> throwR (QueueNotExists qname)
    Just _ -> do
        jes <- getJobs qname mbStatus
        forM jes $ \je -> do
          loadJob (entityKey je)

loadJobsByStatus :: Maybe JobStatus -> DB [JobInfo]
loadJobsByStatus mbStatus = do
  let filt = case mbStatus of
               Nothing -> []
               Just status -> [JobStatus ==. status]
  jes <- selectList filt [Asc JobId]
  forM jes $ \je -> do
      loadJob (entityKey je)

getJobResult :: Int64 -> DB JobResult
getJobResult jid = do
  let jkey = JobKey (SqlBackendKey jid)
  r <- selectFirst [JobResultJobId ==. jkey] [Desc JobResultTime]
  case r of
    Nothing -> throwR JobNotExists
    Just res -> return $ entityVal res

getJobResults :: Int64 -> DB [JobResult]
getJobResults jid = do
  let jkey = JobKey (SqlBackendKey jid)
  res <- selectList [JobResultJobId ==. jkey] [Asc JobResultTime]
  return $ map entityVal res

equals = (E.==.)
infix 4 `equals`

leq = (E.<=.)
infix 4 `leq`

eand = (E.&&.)
infixr 3 `eand`

eor = (E.||.)
infixr 3 `eor`

getLastJobSeq :: String -> DB Int
getLastJobSeq qid = do
  lst <- E.select $
         E.from $ \job -> do
         E.where_ (job ^. JobQueueName `equals` E.val qid)
         return $ E.max_ (job ^. JobSeq)
  case map E.unValue lst of
    (Just r:_) -> return r
    _ -> return 0

getFirstJobSeq :: String -> DB Int
getFirstJobSeq qid = do
  lst <- E.select $
         E.from $ \job -> do
         E.where_ (job ^. JobQueueName `equals` E.val qid)
         return $ E.min_ (job ^. JobSeq)
  case map E.unValue lst of
    (Just r:_) -> return r
    _ -> return 1

getAdjJob :: Bool -> String -> Int -> DB (Maybe Int64, Int)
getAdjJob next qname seq = do
  let cmp = if next then (E.>.) else (E.<.)
  let aggr = if next then E.min_ else E.max_
  lst <- E.select $
         E.from $ \job -> do
         E.where_ ((job ^. JobQueueName `equals` E.val qname) `eand` (job ^. JobSeq `cmp` E.val seq))
         return $ aggr (job ^. JobSeq)
  seq' <- case map E.unValue lst of
            (Just r:_) -> return r
            _ -> return $  if next then (seq+1) else (seq-1)
  mbJob <- getBy (UniqJobSeq qname seq')
  let mbJid = case entityKey `fmap` mbJob of
                Nothing -> Nothing
                Just (JobKey (SqlBackendKey jid)) -> Just jid
  return (mbJid, seq')

getNextJob :: Key Queue -> DB (Maybe JobInfo)
getNextJob (QueueKey qname) = do
  now <- liftIO $ getCurrentTime
  lst <- E.select $
         E.from $ \job -> do
         E.where_ $ (job ^. JobQueueName `equals` E.val qname)
             `eand` (job ^. JobStatus `equals` E.val New)
             `eand` ((E.isNothing (job ^. JobStartTime)) `eor` ((job ^. JobStartTime) `leq` (E.val (Just now))))
         return $ E.min_ (job ^. JobSeq)
  case map E.unValue lst of
    (Just seq:_) -> do
        jinfo <- loadJobSeq qname seq
        return $ Just jinfo
    _ -> return Nothing

deleteQueue :: String -> Bool -> DB ()
deleteQueue name forced = do
  mbQueue <- get (QueueKey name)
  case mbQueue of
    Nothing -> throwR (QueueNotExists name)
    Just qe -> do
      js <- selectFirst [JobQueueName ==. name] []
      if isNothing js || forced
        then delete (QueueKey name)
        else throwR QueueNotEmpty

addQueue :: Queue -> DB (Key Queue)
addQueue q = do
  r <- getQueue (queueName q)
  case r of
    Nothing -> do
        mbSchedule <- get (queueSchedule q)
        case mbSchedule of
          Nothing -> throwR (ScheduleNotExists (queueScheduleName q))
          otherwise -> insert q
    Just _ -> throwR (QueueExists (queueName q))

addQueue' :: String -> String -> DB (Key Queue)
addQueue' name scheduleId = do
  r <- insertUnique $ Queue name name True scheduleId Nothing
  case r of
    Just qid -> return qid
    Nothing -> throwR $ QueueExists name

getQueue :: String -> DB (Maybe Queue)
getQueue name = get (QueueKey name)

updateQueue :: String -> (UpdateList Queue) -> DB ()
updateQueue qname (UpdateList updates) = do
  update (QueueKey qname) updates

getQueueStats :: String -> DB (ByStatus Int)
getQueueStats qname = do
  let sql = "select status, count(1) from job where queue_name = ? group by status"
  pvs <- Sql.rawSql sql [toPersistValue qname]
  return $ ByStatus $ M.fromList [(unSingle st, unSingle cnt) | (st,cnt) <- pvs]

getStats :: DB (M.Map String (ByStatus Int))
getStats = do
  queues <- getAllQueues'
  rs <- forM queues $ \q -> do
            st <- getQueueStats (queueName q)
            return (queueName q, st)
  return $ M.fromList rs

enqueue :: String -> String -> JobInfo -> DB (Key Job)
enqueue username qname jinfo = do
  mbQueue <- getQueue qname
  lockQueue qname
  case mbQueue of
    Nothing -> throwR (QueueNotExists qname)
    Just qe -> do
      seq <- getLastJobSeq qname
      now <- liftIO getCurrentTime
      checkStartTime (jiStartTime jinfo) (queueSchedule qe)
      let job = Job (jiType jinfo) qname (seq+1) username now (jiStartTime jinfo) (jiStatus jinfo) (jiTryCount jinfo) (jiHostName jinfo)
      jid <- insert job
      forM_ (M.assocs $ jiParams jinfo) $ \(name,value) -> do
        let param = JobParam jid name value
        insert_ param
      return jid

checkStartTime :: Maybe UTCTime -> Key Schedule -> DB ()
checkStartTime Nothing _ = return ()
checkStartTime (Just startTime) scheduleId = do
  schedule <- loadSchedule scheduleId
  when (not (schedule `allowsU` startTime)) $
    throwR InvalidStartTime

removeJob :: String -> Int -> DB ()
removeJob qname jseq = do
    deleteCascadeWhere [JobQueueName ==. qname, JobSeq ==. jseq]

removeJobById :: Int64 -> DB ()
removeJobById jid = do
    let key = JobKey (SqlBackendKey jid)
    deleteCascade key

removeJobs :: String -> JobStatus -> DB ()
removeJobs qname status = do
    deleteCascadeWhere [JobQueueName ==. qname, JobStatus ==. status]

cleanupJobResults :: Int -> DB ()
cleanupJobResults days = do
  now <- liftIO $ getCurrentTime
  let delta = fromIntegral $ days * 24 * 3600
  let edge = addUTCTime (negate delta) now
  -- Delete jobs with obsolete results
  -- let deleteParams = "delete from job_param where job_id in (select job_id from job_result where time < ?) and (select status from job where id = job_param.job_id) in ('Done', 'Failed')"
  -- Sql.rawExecute deleteParams [toPersistValue edge]
  -- liftIO $ putStrLn deleteParams
  -- let deleteJobs = "delete from job where id in (select job_id from job_result where time < ?) and status in ('Done', 'Failed')"
  -- liftIO $ putStrLn deleteJobs
  -- Sql.rawExecute deleteJobs [toPersistValue edge]
  -- Delete obsolete job results
  deleteCascadeWhere [JobResultTime <. edge]
  deleteCascadeWhere [JobCreateTime <. edge, JobStatus <-. [Done, Failed]]

-- cleanupJobs :: Int -> DB ()
-- cleanupJobs days = do
--   now <- liftIO $ getCurrentTime
--   let delta = fromIntegral $ days * 3600
--   let edge = addUTCTime (negate delta) now
  -- let deleteResults = "delete from job_result where job_id in (select job_id from job_result where time < ?)"
  -- Sql.rawExecute deleteResults [toPersistValue edge]
  -- let deleteJobs = "delete from job where time 

