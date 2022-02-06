{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Batchd.Daemon.Manager where

import Control.Concurrent
import Control.Monad
import Control.Applicative (optional)
import Control.Monad.Reader
import qualified Control.Monad.State as State
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy hiding (optional)
import Data.Text.Format.Heavy.Parse
import Data.Char (isDigit)
import Data.Maybe
import Data.Default
import Data.Yaml
import Data.Time
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static as Static
import Web.Scotty.Trans as Scotty
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Text as Parsec
import System.FilePath
import System.FilePath.Glob
import System.Log.Heavy.Types
import System.Log.Heavy
import System.Metrics.Json as EKG

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize
import Batchd.Core.Common.Config
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Common.Config
import Batchd.Daemon.Types
import Batchd.Daemon.Database
import Batchd.Daemon.Schedule
import Batchd.Daemon.Auth
import Batchd.Daemon.Monitoring as Monitoring

corsPolicy :: GlobalConfig -> CorsResourcePolicy
corsPolicy cfg =
  let origins = case wcAllowedOrigin =<< (mcWebClient $ dbcManager cfg) of
                  Nothing -> Nothing
                  Just url -> Just ([stringToBstr url], not (isAuthDisabled $ mcAuth $ dbcManager cfg))
  in simpleCorsResourcePolicy {
    corsOrigins = origins,
    corsMethods = ["GET", "POST", "PUT", "DELETE"]
  }

routes :: GlobalConfig -> LoggingTState -> Maybe Wai.Middleware -> ScottyT Error Daemon ()
routes cfg lts mbWaiMetrics = do
  Scotty.defaultHandler raiseError

  Scotty.middleware $ cors $ const $ Just $ corsPolicy cfg
  Scotty.middleware $ requestLogger cfg lts
  case mbWaiMetrics of
    Nothing -> return ()
    Just waiMetrics -> do
      Scotty.middleware waiMetrics
  
  case mcAuth $ dbcManager cfg of
    AuthDisabled -> Scotty.middleware (noAuth cfg lts)
    AuthConfig {..} -> do
      when authHeaderEnabled $
        Scotty.middleware (headerAuth cfg lts)
      when authBasicEnabled $
        Scotty.middleware (basicAuth cfg lts)
    
  case wcPath `fmap` (mcWebClient $ dbcManager cfg) of
    Just path -> do
        Scotty.middleware $ staticPolicy (addBase path)
        Scotty.get "/" $ file $ path </> "batch.html"
        Scotty.get "/monitor" $ file $ path </> "monitor.html"
    Nothing -> return ()

  Scotty.get "/stats" getStatsA
  Scotty.get "/stats/:name" getQueueStatsA

  Scotty.get "/queue" getQueuesA
  Scotty.get "/queue/:name" getQueueA
  Scotty.get "/queue/:name/jobs" getQueueJobsA
  Scotty.get "/queue/:name/type" $ getAllowedJobTypesA
  Scotty.get "/queue/:qname/type/:tname/host" $ getAllowedHostsForTypeA
  Scotty.get "/queue/:name/host" $ getAllowedHostsA
  Scotty.put "/queue/:name" updateQueueA
  Scotty.post "/queue" addQueueA
  Scotty.post "/queue/:name" enqueueA
  Scotty.delete "/queue/:name/:seq" removeJobA
  Scotty.delete "/queue/:name" removeQueueA

  Scotty.get "/job/:id" getJobA
  Scotty.get "/job/:id/results" getJobResultsA
  Scotty.get "/job/:id/results/last" getJobLastResultA
  Scotty.put "/job/:id" updateJobA
  Scotty.delete "/job/:id" removeJobByIdA
  Scotty.get "/jobs" getJobsA

  Scotty.get "/schedule" getSchedulesA
  Scotty.post "/schedule" addScheduleA
  Scotty.delete "/schedule/:name" removeScheduleA

  Scotty.get "/type" $ getJobTypesA
  Scotty.get "/type/:name" getJobTypeA

  Scotty.get "/host" $ getHostsA

  Scotty.post "/user" createUserA
  Scotty.get "/user" getUsersA
  Scotty.put "/user/:name" changePasswordA
  Scotty.get "/user/:name/permissions" getPermissionsA
  Scotty.post "/user/:name/permissions" createPermissionA
  Scotty.delete "/user/:name/permissions/:id" deletePermissionA

  Scotty.get "/monitor/current/tree" currentMetricsTreeA 
  Scotty.get "/monitor/current/plain" currentMetricsPlainA
  Scotty.get "/monitor/jobs" getQueueJobsCountA
  Scotty.get "/monitor/:prefix/current/tree" currentMetricsTreeA 
  Scotty.get "/monitor/:prefix/current/plain" currentMetricsPlainA
  Scotty.get "/monitor/:name/last" lastMetricA 
  Scotty.get "/monitor/:prefix/query" queryMetricA

  Scotty.options "/" $ getAuthOptionsA
  Scotty.options (Scotty.regex "/.*") $ done

requestLogger :: GlobalConfig -> LoggingTState -> Wai.Middleware
requestLogger gcfg lts app req sendResponse = do
  -- putStrLn "request logger"
  debugIO lts $(here) "Request: {method} {path}. User-Agent: {useragent}" req
  app req sendResponse

runManager :: Daemon ()
runManager = do
    connInfo <- Daemon $ lift State.get
    cfg <- askConfig
    let options = def {Scotty.settings = setPort (mcPort $ dbcManager cfg) defaultSettings}
    lts <- askLoggingStateM
    waiMetrics <- getWaiMetricsMiddleware
    forkDaemon "job metrics calculator" jobMetricsCalculator
    forkDaemon "metrics dumper"         metricsDumper
    forkDaemon "queue runner"           queueRunner
    forkDaemon "maintainer"             maintainer
    forkDaemon "metrics cleaner"        metricsCleaner
    liftIO $ do
      scottyOptsT options (runService connInfo lts) $ routes cfg lts waiMetrics
  where
    runService connInfo lts actions =
        runDaemonIO connInfo lts $
          withLogVariable "thread" ("REST service" :: String) $ actions

jobMetricsCalculator :: Daemon ()
jobMetricsCalculator = forever $ do
    liftIO $ threadDelay $ 60 * 1000*1000
    r <- runDB getStats
    case r of
      Left err -> $reportError "Can't get job statistics" ()
      Right byQueue -> do
        forM_ (M.assocs byQueue) $ \(name, ByStatus byStatus) -> do
          forM_ (M.assocs byStatus) $ \(status, count) -> do
            let gname = "batchd.queue." <> T.pack name <> ".jobs." <> T.pack (show status)
            Monitoring.gauge gname count
        let totals = M.unionsWith (+) [byStatus | ByStatus byStatus <- M.elems byQueue]
        forM_ (M.assocs totals) $ \(status, count) -> do
          let gname = "batchd.jobs." <> T.pack (show status)
          Monitoring.gauge gname count

maintainer :: Daemon ()
maintainer = forever $ do
  cfg <- askConfig
  runDB $ cleanupJobResults (scDoneJobs $ dbcStorage cfg)
  liftIO $ threadDelay $ 60 * 1000*1000

queueRunner :: Daemon ()
queueRunner = forever $ do
  qesr <- runDB getDisabledQueues'
  case qesr of
    Left err -> $reportError "Can't get list of disabled queues: {}" (Single $ show err)
    Right queues -> do
      forM_ queues $ \queue -> do
        case queueAutostartJobCount queue of
          Nothing -> return ()
          Just minCount -> do
            countRes <- runDB $ getNewJobsCount (queueName queue)
            case countRes of
              Left err -> $reportError "Can't get list of new jobs in queue `{}': {}" (queueName queue, show err)
              Right currentCount -> do
                if currentCount >= minCount
                  then do
                       $debug "Disabled queue `{}' has {} new jobs, enable it" (queueName queue, currentCount)
                       r <- runDB $ enableQueue (queueName queue)
                       case r of
                         Left err -> $reportError "Can't enable queue `{}': {}" (queueName queue, show err)
                         Right _ -> return ()
                  else $debug "Disabled queue `{}' has {} new jobs, do not enable it" (queueName queue, currentCount)
  liftIO $ threadDelay $ 60 * 1000*1000

-- | Get URL parameter in form ?name=value
getUrlParam :: B.ByteString -> Action (Maybe B.ByteString)
getUrlParam key = do
  rq <- Scotty.request
  let qry = Wai.queryString rq
  return $ join $ lookup key qry

raise404 :: Action TL.Text -> Maybe String -> Action ()
raise404 message mbs = do
  localizedMessage <- message
  Scotty.status status404
  case mbs of
    Nothing -> Scotty.text localizedMessage
    Just name -> do
      let msg' = format (parseFormat' localizedMessage) (Single name)
      Scotty.text msg'

raiseError :: Error -> Action ()
raiseError (QueueNotExists name) = raise404 (__ "Queue not found: `{}'") (Just name)
raiseError JobNotExists   = raise404 (__ "Specified job not found") Nothing
raiseError (FileNotExists name)  = raise404 (__ "File not found: `{}'") (Just name)
raiseError (MetricNotExists name)  = raise404 (__ "Metric not found: `{}'") (Just name)
raiseError QueueNotEmpty  = Scotty.status status403
raiseError (InsufficientRights msg) = do
  Scotty.status status403
  Scotty.text $ TL.pack msg
raiseError e = do
  Scotty.status status500
  Scotty.text $ TL.pack $ show e

done :: Action ()
done = Scotty.json ("done" :: String)

inUserContext :: Action a -> Action a
inUserContext action = do
  name <- getAuthUserName
  withLogVariable "user" (fromMaybe "<unauthorized>" name) $ do
    -- req <- Scotty.request
    -- context <- getLogContext
    -- liftIO $ putStrLn $ show context
    -- \$debug "Request: {method} {path}. User-Agent: {useragent}" req
    action

getQueuesA :: Action ()
getQueuesA = inUserContext $ do
  $debug "getting queues list" ()
  user <- getAuthUser
  let name = userName user
  super <- isSuperUser name
  qes <- if super
           then runDBA getAllQueues'
           else runDBA $ getAllowedQueues name ViewQueues
  Scotty.json qes

getQueueA :: Action ()
getQueueA = inUserContext $ do
  qname <- Scotty.param "name"
  checkPermission "view queue" ViewQueues qname
  mbQueue <- runDBA $ getQueue qname
  case mbQueue of
    Nothing -> raise (QueueNotExists qname)
    Just queue -> Scotty.json queue

parseStatus' :: Maybe JobStatus -> Maybe B.ByteString -> Action (Maybe JobStatus)
parseStatus' dflt str = parseStatus dflt (raise (InvalidJobStatus str)) str

getQueueJobsA :: Action ()
getQueueJobsA = inUserContext $ do
  qname <- Scotty.param "name"
  checkPermission "view queue jobs" ViewJobs qname
  st <- getUrlParam "status"
  fltr <- parseStatus' (Just New) st
  jobs <- runDBA $ loadJobs qname fltr
  Scotty.json jobs

getQueueStatsA :: Action ()
getQueueStatsA = inUserContext $ do
  qname <- Scotty.param "name"
  checkPermission "view queue statistics" ManageJobs qname
  stats <- runDBA $ getQueueStats qname
  Scotty.json stats

getStatsA :: Action ()
getStatsA = inUserContext $ do
  checkPermissionToList "view queue statistics" ManageJobs
  stats <- runDBA getStats
  Scotty.json stats

enqueueA :: Action ()
enqueueA = inUserContext $ do
  jinfo <- jsonData
  qname <- Scotty.param "name"
  user <- getAuthUser
  -- special name for default host of queue
  let hostToCheck = fromMaybe defaultHostOfQueue (T.pack <$> jiHostName jinfo)
  checkCanCreateJobs qname (jiType jinfo) hostToCheck
  r <- runDBA $ enqueue (userName user) qname jinfo
  Scotty.json r

removeJobA :: Action ()
removeJobA = inUserContext $ do
  qname <- Scotty.param "name"
  checkPermission "delete jobs from queue" ManageJobs qname
  jseq <- Scotty.param "seq"
  runDBA $ removeJob qname jseq
  done

removeJobByIdA :: Action ()
removeJobByIdA = inUserContext $ do
  checkPermissionToList "delete jobs" ManageJobs
  jid <- Scotty.param "id"
  runDBA $ removeJobById jid
  done

getJobLastResultA :: Action ()
getJobLastResultA = inUserContext $ do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "view job result" ViewJobs (jiQueue job)
  res <- runDBA $ getJobResult jid
  Scotty.json res

getJobResultsA :: Action ()
getJobResultsA = inUserContext $ do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "view job result" ViewJobs (jiQueue job)
  res <- runDBA $ getJobResults jid
  Scotty.json res

removeQueueA :: Action ()
removeQueueA = inUserContext $ do
  qname <- Scotty.param "name"
  checkPermission "delete queue" ManageQueues qname
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
        Right _ -> done
    Just status -> do
        runDBA $ removeJobs qname status
        done

getSchedulesA :: Action ()
getSchedulesA = inUserContext $ do
  checkPermissionToList "get list of schedules" ViewSchedules
  ss <- runDBA loadAllSchedules
  Scotty.json ss

addScheduleA :: Action ()
addScheduleA = inUserContext $ do
  checkPermissionToList "create schedule" ManageSchedules
  sd <- jsonData
  name <- runDBA $ addSchedule sd
  Scotty.json name

removeScheduleA :: Action ()
removeScheduleA = inUserContext $ do
  checkPermissionToList "delete schedule" ManageSchedules
  name <- Scotty.param "name"
  forced <- getUrlParam "forced"
  r <- runDBA' $ removeSchedule name (forced == Just "true")
  case r of
    Left ScheduleUsed -> Scotty.status status403
    Left e -> Scotty.raise e
    Right _ -> done

addQueueA :: Action ()
addQueueA = inUserContext $ do
  checkPermissionToList "create queue" ManageQueues
  qd <- jsonData
  name <- runDBA $ addQueue qd
  Scotty.json name

updateQueueA :: Action ()
updateQueueA = inUserContext $ do
  name <- Scotty.param "name"
  checkPermission "modify queue" ManageQueues name
  upd <- jsonData
  runDBA $ updateQueue name upd
  done

getJobA :: Action ()
getJobA = inUserContext $ do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "view job" ViewJobs (jiQueue job)
  Scotty.json job

updateJobA :: Action ()
updateJobA = inUserContext $ do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "modify job" ManageJobs (jiQueue job)
  qry <- jsonData
  runDBA $ case qry of
            UpdateJob upd -> updateJob jid upd
            Move qname -> moveJob jid qname
            Prioritize action -> prioritizeJob jid action
  done

getJobsA :: Action ()
getJobsA = inUserContext $ do
  checkPermissionToList "view jobs from all queues" ViewJobs
  st <- getUrlParam "status"
  fltr <- parseStatus' (Just New) st
  jobs <- runDBA $ loadJobsByStatus fltr
  Scotty.json jobs

deleteJobsA :: Action ()
deleteJobsA = inUserContext $ do
  name <- Scotty.param "name"
  checkPermission "delete jobs" ManageJobs name
  st <- getUrlParam "status"
  fltr <- parseStatus' Nothing st
  case fltr of
    Nothing -> raise $ InvalidJobStatus st
    Just status -> runDBA $ removeJobs name status

getJobTypesA :: Action ()
getJobTypesA = inUserContext $ do
  cfg <- askConfigA
  lts <- askLoggingState
  types <- liftIO $ listJobTypes cfg lts
  Scotty.json types

getAllowedJobTypesA :: Action ()
getAllowedJobTypesA = inUserContext $ do
  user <- getAuthUser
  qname <- Scotty.param "name"
  let name = userName user
  cfg <- askConfigA
  lts <- askLoggingState
  types <- liftIO $ listJobTypes cfg lts
  allowedTypes <- flip filterM types $ \jt -> do
                      hasCreatePermission name qname (Just $ jtName jt) Nothing
  Scotty.json allowedTypes

listJobTypes :: GlobalConfig -> LoggingTState -> IO [JobType]
listJobTypes cfg lts = do
  dirs <- getConfigDirs "jobtypes"
  files <- forM dirs $ \dir -> glob (dir </> "*.yaml")
  ts <- forM (concat files) $ \path -> do
             r <- decodeFileEither path
             case r of
               Left err -> do
                  reportErrorIO lts $(here) "Can't parse job type description file: {}" (Single $ Shown err)
                  return []
               Right jt -> return [jt]
  let types = concat ts :: [JobType]
  return types

getJobTypeA :: Action ()
getJobTypeA = inUserContext $ do
  name <- Scotty.param "name"
  r <- liftIO $ loadTemplate name
  case r of
    Left err -> raise err
    Right jt -> Scotty.json jt

listHosts :: GlobalConfig -> LoggingTState -> IO [Host]
listHosts cfg lts = do
  dirs <- getConfigDirs "hosts"
  files <- forM dirs $ \dir -> glob (dir </> "*.yaml")
  hs <- forM (concat files) $ \path -> do
             r <- decodeFileEither path
             case r of
               Left err -> do
                  reportErrorIO lts $(here) "Can't parse host description file: {}" (Single $ Shown err)
                  return []
               Right host -> return [host]
  let hosts = concat hs :: [Host]
  return hosts

getHostsA :: Action ()
getHostsA = inUserContext $ do
  cfg <- askConfigA
  lts <- askLoggingState
  hosts <- liftIO $ listHosts cfg lts
  Scotty.json $ map hName hosts

getAllowedHostsA :: Action ()
getAllowedHostsA = inUserContext $ do
  user <- getAuthUser
  qname <- Scotty.param "name"
  let name = userName user
  cfg <- askConfigA
  lts <- askLoggingState
  hosts <- liftIO $ listHosts cfg lts
  let allHostNames = defaultHostOfQueue : map hName hosts
  allowedHosts <- flip filterM allHostNames $ \hostname -> do
                      hasCreatePermission name qname Nothing (Just hostname)
  Scotty.json allowedHosts

getAllowedHostsForTypeA :: Action ()
getAllowedHostsForTypeA = inUserContext $ do
  user <- getAuthUser
  qname <- Scotty.param "qname"
  tname <- Scotty.param "tname"
  mbAllowedHosts <- listAllowedHosts (userName user) qname tname
  allowedHosts <- case mbAllowedHosts of
                    Just list -> return list -- user is restricted to list of hosts
                    Nothing -> do -- user can create jobs on any defined host
                        cfg <- askConfigA
                        lts <- askLoggingState
                        hosts <- liftIO $ listHosts cfg lts
                        return $ defaultHostOfQueue : map hName hosts
  Scotty.json allowedHosts

getUsersA :: Action ()
getUsersA = inUserContext $ do
  checkSuperUser
  names <- runDBA getUsers
  Scotty.json names

createUserA :: Action ()
createUserA = inUserContext $ do
  checkSuperUser
  user <- jsonData
  cfg <- askConfigA
  let staticSalt = getAuthStaticSalt $ mcAuth $ dbcManager cfg
  name <- runDBA $ createUserDb (uiName user) (uiPassword user) staticSalt
  Scotty.json name

changePasswordA :: Action ()
changePasswordA = inUserContext $ do
  name <- Scotty.param "name"
  curUser <- getAuthUser
  when (userName curUser /= name) $
      checkSuperUser
  user <- jsonData
  cfg <- askConfigA
  let staticSalt = getAuthStaticSalt $ mcAuth $ dbcManager cfg
  runDBA $ changePasswordDb name (uiPassword user) staticSalt
  done

createPermissionA :: Action ()
createPermissionA = inUserContext $ do
  checkSuperUser
  name <- Scotty.param "name"
  perm <- jsonData
  id <- runDBA $ createPermission name perm
  Scotty.json id

getPermissionsA :: Action ()
getPermissionsA = inUserContext $ do
  checkSuperUser
  name <- Scotty.param "name"
  perms <- runDBA $ getPermissions name
  Scotty.json perms

deletePermissionA :: Action ()
deletePermissionA = inUserContext $ do
  checkSuperUser
  name <- Scotty.param "name"
  id <- Scotty.param "id"
  runDBA $ deletePermission id name
  done

getAuthOptionsA :: Action ()
getAuthOptionsA = inUserContext $ do
  cfg <- askConfigA
  let methods = authMethods $ mcAuth $ dbcManager cfg
  Scotty.json methods

currentMetricsPlainA :: Action ()
currentMetricsPlainA = inUserContext $ do
  mbPrefix <- optional $ Scotty.param "prefix"
  metrics <- lift $ getCurrentMetrics mbPrefix
  now <- liftIO $ getCurrentTime
  Scotty.json $ sampleToJsonPlain now metrics

currentMetricsTreeA :: Action ()
currentMetricsTreeA = inUserContext $ do
  mbPrefix <- optional $ Scotty.param "prefix"
  metrics <- lift $ getCurrentMetrics mbPrefix
  Scotty.json $ EKG.sampleToJson metrics

lastMetricA :: Action ()
lastMetricA = inUserContext $ do
  name <- Scotty.param "name"
  metric <- runDBA $ getLastMetric name
  Scotty.json $ metricRecordToJsonTree metric

queryMetricA :: Action ()
queryMetricA = inUserContext $ do
    prefix <- Scotty.param "prefix"
    (from, to) <- getPeriod
    records <- runDBA $ queryMetric (prefix <> "%") from to
    Scotty.json $ map metricRecordToJsonPlain records

getPeriod :: Action (UTCTime, UTCTime)
getPeriod = do
    mbLast <- getUrlParam "last"
    mbFrom <- getUrlParam "from"
    mbTo <- getUrlParam "to"
    now <- liftIO $ getCurrentTime
    case (mbFrom, mbTo, mbLast) of
      (Just f, Nothing, Nothing) -> liftM2 (,) (parse f) (return now)
      (Just f, Just t, Nothing) -> liftM2 (,) (parse f) (parse t)
      (Nothing, Nothing, Just l) -> getLast l now
      _ -> raise $ UnknownError "Time period is not specified for metrics request"

  where

    parse :: B.ByteString -> Action UTCTime
    parse bstr = do
      let str = bstrToString bstr
      local <- case readSTime True defaultTimeLocale "%FT%T" str of
                [(result, "")] -> return result
                _ -> raise $ UnknownError $ "Can't parse time: " ++ str 
      tz <- liftIO $ getCurrentTimeZone
      return $ localTimeToUTC tz local

    getLast :: B.ByteString -> UTCTime -> Action (UTCTime, UTCTime)
    getLast bstr now = do
      let str = bstrToString bstr
      seconds <- if all isDigit str
                   then return $ read str
                   else raise $ UnknownError $ "Can't parse period: " ++ str
      let start = addUTCTime (fromIntegral $ negate seconds) now
      return (start, now)

getQueueJobsCountA :: Action ()
getQueueJobsCountA = do
    (from, to) <- getPeriod
    records <- runDBA $ queryMetric "batchd.queue.%.jobs.%" from to
    result <- forM records parseRecord
    Scotty.json result
  where
    
    parseRecord :: MetricRecord -> Action Data.Yaml.Value
    parseRecord r = do
      (queue, status) <- parseName (metricRecordName r)
      return $ object [
        "time" .= metricRecordTime r,
        "queue" .= queue,
        "status" .= status,
        "jobs" .= metricRecordValue r]
    
    parseName :: T.Text -> Action (T.Text, JobStatus)
    parseName name = case Parsec.runParser pName () "<metric name>" name of
                       Left err -> raise $ UnknownError $ show err
                       Right res -> return res

    pName :: Parsec.Parser (T.Text, JobStatus)
    pName = do
      Parsec.string "batchd.queue."
      queue <- Parsec.many1 (Parsec.noneOf ".")
      Parsec.string ".jobs."
      status <- Parsec.many1 Parsec.alphaNum
      return (T.pack queue, read status)
    
