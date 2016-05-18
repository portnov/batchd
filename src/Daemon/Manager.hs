{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Daemon.Manager where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Control.Monad.State as State
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import Data.Maybe
import Data.Default
import Data.Yaml
import Network.HTTP.Types
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Static as Static
import Web.Scotty.Trans as Scotty
import System.FilePath
import System.FilePath.Glob
import System.Log.Heavy (Logger)

import Common.Types
import Common.Config
import Common.Data
import Daemon.Types
import Daemon.Database
import Daemon.Schedule
import Daemon.Auth
import Daemon.Logging

corsPolicy :: GlobalConfig -> CorsResourcePolicy
corsPolicy cfg =
  let origins = case dbcAllowedOrigin cfg of
                  Nothing -> Nothing
                  Just url -> Just ([stringToBstr url], not (isAuthDisabled $ dbcAuth cfg))
  in simpleCorsResourcePolicy {
    corsOrigins = origins,
    corsMethods = ["GET", "POST", "PUT", "DELETE"]
  }

routes :: GlobalConfig -> Logger -> ScottyT Error Daemon ()
routes cfg logger = do
  Scotty.defaultHandler raiseError

  Scotty.middleware $ cors $ const $ Just $ corsPolicy cfg
  
  case dbcAuth cfg of
    AuthDisabled -> Scotty.middleware (noAuth cfg logger)
    AuthConfig {..} -> do
      when authHeaderEnabled $
        Scotty.middleware (headerAuth cfg logger)
      when authBasicEnabled $
        Scotty.middleware (basicAuth cfg logger)
    
  case dbcWebClientPath cfg of
    Just path -> do
        Scotty.middleware $ staticPolicy (addBase path)
        Scotty.get "/" $ file $ path </> "batch.html"
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

  Scotty.options "/" $ getAuthOptionsA
  Scotty.options (Scotty.regex "/.*") $ done

runManager :: Daemon ()
runManager = do
  connInfo <- Daemon $ lift State.get
  cfg <- askConfig
  let options = def {Scotty.settings = setPort (dbcManagerPort cfg) defaultSettings}
  logger <- askLoggerM
  liftIO $ do
    forkIO $ runDaemonIO connInfo logger maintainer
    scottyOptsT options (runDaemonIO connInfo logger) $ routes cfg logger

maintainer :: Daemon ()
maintainer = forever $ do
  cfg <- askConfig
  runDB $ cleanupJobResults (dbcStoreDone cfg)
  liftIO $ threadDelay $ 60 * 1000*1000

-- | Get URL parameter in form ?name=value
getUrlParam :: B.ByteString -> Action (Maybe B.ByteString)
getUrlParam key = do
  rq <- Scotty.request
  let qry = Wai.queryString rq
  return $ join $ lookup key qry

raise404 :: String -> Maybe String -> Action ()
raise404 t mbs = do
  Scotty.status status404
  case mbs of
    Nothing -> Scotty.text $ TL.pack $ "Specified " ++ t ++ " not found."
    Just name -> Scotty.text $ TL.pack $ "Specified " ++ t ++ " not found: " ++ name

raiseError :: Error -> Action ()
raiseError (QueueNotExists name) = raise404 "queue" (Just name)
raiseError JobNotExists   = raise404 "job" Nothing
raiseError (FileNotExists name)  = raise404 "file" (Just name)
raiseError QueueNotEmpty  = Scotty.status status403
raiseError (InsufficientRights msg) = do
  Scotty.status status403
  Scotty.text $ TL.pack msg
raiseError e = do
  Scotty.status status500
  Scotty.text $ TL.pack $ show e

done :: Action ()
done = Scotty.json ("done" :: String)

getQueuesA :: Action ()
getQueuesA = do
  user <- getAuthUser
  let name = userName user
  cfg <- askConfigA
  qes <- runDBA $ do
           super <- isSuperUser name
           if super || isAuthDisabled (dbcAuth cfg)
             then getAllQueues'
             else getAllowedQueues name ViewQueues
  -- let qnames = map (queueName . entityVal) qes
  Scotty.json qes

getQueueA :: Action ()
getQueueA = do
  qname <- Scotty.param "name"
  checkPermission "view queue" ViewQueues qname
  mbQueue <- runDBA $ getQueue qname
  case mbQueue of
    Nothing -> raise (QueueNotExists qname)
    Just queue -> Scotty.json queue

parseStatus' :: Maybe JobStatus -> Maybe B.ByteString -> Action (Maybe JobStatus)
parseStatus' dflt str = parseStatus dflt (raise (InvalidJobStatus str)) str

getQueueJobsA :: Action ()
getQueueJobsA = do
  qname <- Scotty.param "name"
  checkPermission "view queue jobs" ViewJobs qname
  st <- getUrlParam "status"
  fltr <- parseStatus' (Just New) st
  jobs <- runDBA $ loadJobs qname fltr
  Scotty.json jobs

getQueueStatsA :: Action ()
getQueueStatsA = do
  qname <- Scotty.param "name"
  checkPermission "view queue statistics" ManageJobs qname
  stats <- runDBA $ getQueueStats qname
  Scotty.json stats

getStatsA :: Action ()
getStatsA = do
  checkPermissionToList "view queue statistics" ManageJobs
  stats <- runDBA getStats
  Scotty.json stats

enqueueA :: Action ()
enqueueA = do
  jinfo <- jsonData
  qname <- Scotty.param "name"
  user <- getAuthUser
  -- special name for default host of queue
  let hostToCheck = fromMaybe defaultHostOfQueue (jiHostName jinfo)
  checkCanCreateJobs qname (jiType jinfo) hostToCheck
  r <- runDBA $ enqueue (userName user) qname jinfo
  Scotty.json r

removeJobA :: Action ()
removeJobA = do
  qname <- Scotty.param "name"
  checkPermission "delete jobs from queue" ManageJobs qname
  jseq <- Scotty.param "seq"
  runDBA $ removeJob qname jseq
  done

removeJobByIdA :: Action ()
removeJobByIdA = do
  checkPermissionToList "delete jobs" ManageJobs
  jid <- Scotty.param "id"
  runDBA $ removeJobById jid
  done

getJobLastResultA :: Action ()
getJobLastResultA = do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "view job result" ViewJobs (jiQueue job)
  res <- runDBA $ getJobResult jid
  Scotty.json res

getJobResultsA :: Action ()
getJobResultsA = do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "view job result" ViewJobs (jiQueue job)
  res <- runDBA $ getJobResults jid
  Scotty.json res

removeQueueA :: Action ()
removeQueueA = do
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
getSchedulesA = do
  checkPermissionToList "get list of schedules" ViewSchedules
  ss <- runDBA loadAllSchedules
  Scotty.json ss

addScheduleA :: Action ()
addScheduleA = do
  checkPermissionToList "create schedule" ManageSchedules
  sd <- jsonData
  name <- runDBA $ addSchedule sd
  Scotty.json name

removeScheduleA :: Action ()
removeScheduleA = do
  checkPermissionToList "delete schedule" ManageSchedules
  name <- Scotty.param "name"
  forced <- getUrlParam "forced"
  r <- runDBA' $ removeSchedule name (forced == Just "true")
  case r of
    Left ScheduleUsed -> Scotty.status status403
    Left e -> Scotty.raise e
    Right _ -> done

addQueueA :: Action ()
addQueueA = do
  checkPermissionToList "create queue" ManageQueues
  qd <- jsonData
  name <- runDBA $ addQueue qd
  Scotty.json name

updateQueueA :: Action ()
updateQueueA = do
  name <- Scotty.param "name"
  checkPermission "modify queue" ManageQueues name
  upd <- jsonData
  runDBA $ updateQueue name upd
  done

getJobA :: Action ()
getJobA = do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "view job" ViewJobs (jiQueue job)
  Scotty.json job

updateJobA :: Action ()
updateJobA = do
  jid <- Scotty.param "id"
  job <- runDBA $ loadJob' jid
  checkPermission "modify job" ManageJobs (jiQueue job)
  qry <- jsonData
  runDBA $ case qry of
            UpdateJob upd -> updateJob job upd
            Move qname -> moveJob jid qname
            Prioritize action -> prioritizeJob jid action
  done

getJobsA :: Action ()
getJobsA = do
  checkPermissionToList "view jobs from all queues" ViewJobs
  st <- getUrlParam "status"
  fltr <- parseStatus' (Just New) st
  jobs <- runDBA $ loadJobsByStatus fltr
  Scotty.json jobs

deleteJobsA :: Action ()
deleteJobsA = do
  name <- Scotty.param "name"
  checkPermission "delete jobs" ManageJobs name
  st <- getUrlParam "status"
  fltr <- parseStatus' Nothing st
  case fltr of
    Nothing -> raise $ InvalidJobStatus st
    Just status -> runDBA $ removeJobs name status

getJobTypesA :: Action ()
getJobTypesA = do
  cfg <- askConfigA
  logger <- askLogger
  types <- liftIO $ listJobTypes cfg logger
  Scotty.json types

getAllowedJobTypesA :: Action ()
getAllowedJobTypesA = do
  user <- getAuthUser
  qname <- Scotty.param "name"
  let name = userName user
  cfg <- askConfigA
  logger <- askLogger
  types <- liftIO $ listJobTypes cfg logger
  allowedTypes <- flip filterM types $ \jt -> do
                      runDBA $ hasCreatePermission name qname (Just $ jtName jt) Nothing
  Scotty.json allowedTypes

listJobTypes :: GlobalConfig -> Logger -> IO [JobType]
listJobTypes cfg logger = do
  dirs <- getConfigDirs "jobtypes"
  files <- forM dirs $ \dir -> glob (dir </> "*.yaml")
  ts <- forM (concat files) $ \path -> do
             r <- decodeFileEither path
             case r of
               Left err -> do
                  reportErrorIO logger $(here) $ show err
                  return []
               Right jt -> return [jt]
  let types = concat ts :: [JobType]
  return types

getJobTypeA :: Action ()
getJobTypeA = do
  name <- Scotty.param "name"
  r <- liftIO $ loadTemplate name
  case r of
    Left err -> raise err
    Right jt -> Scotty.json jt

listHosts :: GlobalConfig -> Logger -> IO [Host]
listHosts cfg logger = do
  dirs <- getConfigDirs "hosts"
  files <- forM dirs $ \dir -> glob (dir </> "*.yaml")
  hs <- forM (concat files) $ \path -> do
             r <- decodeFileEither path
             case r of
               Left err -> do
                  reportErrorIO logger $(here) $ show err
                  return []
               Right host -> return [host]
  let hosts = concat hs :: [Host]
  return hosts

getHostsA :: Action ()
getHostsA = do
  cfg <- askConfigA
  logger <- askLogger
  hosts <- liftIO $ listHosts cfg logger
  Scotty.json $ map hName hosts

getAllowedHostsA :: Action ()
getAllowedHostsA = do
  user <- getAuthUser
  qname <- Scotty.param "name"
  let name = userName user
  cfg <- askConfigA
  logger <- askLogger
  hosts <- liftIO $ listHosts cfg logger
  let allHostNames = defaultHostOfQueue : map hName hosts
  allowedHosts <- flip filterM allHostNames $ \hostname -> do
                      runDBA $ hasCreatePermission name qname Nothing (Just hostname)
  Scotty.json allowedHosts

getAllowedHostsForTypeA :: Action ()
getAllowedHostsForTypeA = do
  user <- getAuthUser
  qname <- Scotty.param "qname"
  tname <- Scotty.param "tname"
  mbAllowedHosts <- runDBA $ listAllowedHosts (userName user) qname tname
  allowedHosts <- case mbAllowedHosts of
                    Just list -> return list -- user is restricted to list of hosts
                    Nothing -> do -- user can create jobs on any defined host
                        cfg <- askConfigA
                        logger <- askLogger
                        hosts <- liftIO $ listHosts cfg logger
                        return $ defaultHostOfQueue : map hName hosts
  Scotty.json allowedHosts

getUsersA :: Action ()
getUsersA = do
  checkSuperUser
  names <- runDBA getUsers
  Scotty.json names

createUserA :: Action ()
createUserA = do
  checkSuperUser
  user <- jsonData
  cfg <- askConfigA
  let staticSalt = authStaticSalt $ dbcAuth cfg
  name <- runDBA $ createUserDb (uiName user) (uiPassword user) staticSalt
  Scotty.json name

changePasswordA :: Action ()
changePasswordA = do
  name <- Scotty.param "name"
  curUser <- getAuthUser
  when (userName curUser /= name) $
      checkSuperUser
  user <- jsonData
  cfg <- askConfigA
  let staticSalt = authStaticSalt $ dbcAuth cfg
  runDBA $ changePassword name (uiPassword user) staticSalt
  done

createPermissionA :: Action ()
createPermissionA = do
  checkSuperUser
  name <- Scotty.param "name"
  perm <- jsonData
  id <- runDBA $ createPermission name perm
  Scotty.json id

getPermissionsA :: Action ()
getPermissionsA = do
  checkSuperUser
  name <- Scotty.param "name"
  perms <- runDBA $ getPermissions name
  Scotty.json perms

deletePermissionA :: Action ()
deletePermissionA = do
  checkSuperUser
  name <- Scotty.param "name"
  id <- Scotty.param "id"
  runDBA $ deletePermission id name
  done

getAuthOptionsA :: Action ()
getAuthOptionsA = do
  cfg <- askConfigA
  let methods = authMethods $ dbcAuth cfg
  Scotty.json methods

