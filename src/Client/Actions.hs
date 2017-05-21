{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module Client.Actions where

import Control.Monad
import Control.Monad.State
import Data.Int
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Char
import Data.List (intercalate)
import Data.Aeson
import System.FilePath
import Text.Printf

import Common.Types
import qualified Common.Data as Database
import Common.Schedule
import Common.Config
import Client.CmdLine
import Client.Config
import Client.Http
import Client.Monad

doEnqueue :: Client ()
doEnqueue = do
  cfg <- gets csConfig
  baseUrl <- getBaseUrl
  creds <- getCredentials
  opts <- gets csCmdline
  t <- liftIO $ getTypeName opts cfg
  jtr <- liftIO $ loadTemplate t
  case jtr of
    Left err -> throwC $ show err
    Right jtype -> do
      qname <- liftIO $ getQueueName opts cfg
      host <- liftIO $ getHostName opts cfg

      let job = JobInfo {
          jiId = 0,
          jiQueue = qname,
          jiType = t,
          jiSeq = 0,
          jiUserName = fst creds,
          jiCreateTime = zeroUtcTime,
          jiStatus = New,
          jiTryCount = 0,
          jiHostName = host,
          jiResultTime = Nothing,
          jiExitCode = Nothing,
          jiStdout = Nothing,
          jiStderr = Nothing,
          jiParams = parseParams (jtParams jtype) (cmdCommand opts)
        }

      let url = baseUrl </> "queue" </> qname
      doPost url job

doList :: Client ()
doList = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  case queueToList command of
    [] -> do
      let url = baseUrl </> "queue"
      response <- doGet url
      forM_ (response :: [Database.Queue]) $ \queue -> do
        liftIO $ printf "[%s]\t%s:\t%s\t%s\t%s\n"
                   ((if Database.queueEnabled queue then "*" else " ") :: String)
                   (Database.queueName queue)
                   (Database.queueTitle queue)
                   (Database.queueScheduleName queue)
                   (fromMaybe "*" $ Database.queueHostName queue)

    qnames ->
      forM_ qnames $ \qname -> do
        statusOpt <- parseStatus (Just New) (throwC "Invalid status") (status command)
        let statusStr = case statusOpt of
                          Nothing -> "?status=all"
                          Just st -> case status command of
                                       Nothing -> ""
                                       _ -> "?status=" ++ map toLower (show st)
        let url = baseUrl </> "queue" </> qname </> "jobs" ++ statusStr
        response <- doGet url
        liftIO $ forM_ (response :: [JobInfo]) $ \job -> do
                  printf "#%d: [%d]\t%s\t%s\n" (jiId job) (jiSeq job) (jiType job) (show $ jiStatus job)
                  forM_ (M.assocs $ jiParams job) $ \(name, value) -> do
                    printf "\t%s:\t%s\n" name value

doStats :: Client ()
doStats = do
    baseUrl <- getBaseUrl
    opts <- gets csCmdline
    let command = cmdCommand opts
    case queueToStat command of
      [] -> do
        let url = baseUrl </> "stats"
        response <- doGet url
        liftIO $ forM_ (M.assocs response) $ \rec -> do
                  let qname = fst rec :: String
                      stat = snd rec :: ByStatus Int
                  liftIO $ putStrLn $ qname ++ ":"
                  liftIO $ printStats stat
      qnames -> do
        forM_ qnames $ \qname -> do
          liftIO $ putStrLn $ qname ++ ":"
          let url = baseUrl </> "stats" </> qname
          response <- doGet url
          liftIO $ printStats response
  where
    printStats :: ByStatus Int -> IO ()
    printStats (ByStatus stat) =
      forM_ (M.assocs stat) $ \(st, cnt) ->
          printf "\t%s:\t%d\n" (show st) cnt

viewJob :: Client ()
viewJob = do
    baseUrl <- getBaseUrl
    opts <- gets csCmdline
    let command = cmdCommand opts
    let showDescription = if viewDescription command
                            then True
                            else if not (viewResult command) && not (viewAll command)
                                   then True
                                   else viewDescription command
    if (showDescription || viewResult command) && not (viewAll command)
      then do
           let url = baseUrl </> "job" </> show (jobId command)
           response <- doGet url
           when (showDescription) $
               liftIO $ printJob response
           when (viewResult command) $
               liftIO $ printLastResult response
      else do
           when (showDescription) $ do
               let url = baseUrl </> "job" </> show (jobId command)
               response <- doGet url
               liftIO $ printJob response
           when (viewAll command) $ do
               let url = baseUrl </> "job" </> show (jobId command) </> "results"
               response <- doGet url
               forM_ (response :: [Database.JobResult]) $ \result -> do
                   liftIO $ printResult result
  where
    printJob :: JobInfo -> IO ()
    printJob job = do
      let host = fromMaybe "*" $ jiHostName job
      printf "Order:\t%d\nType:\t%s\nQueue:\t%s\nHost:\t%s\nUser:\t%s\nCreated:\t%s\nStatus:\t%s\nTry count:\t%d\n"
        (jiSeq job) (jiType job) (jiQueue job) host (jiUserName job) (show $ jiCreateTime job)
        (show $ jiStatus job) (jiTryCount job)
      forM_ (M.assocs $ jiParams job) $ \(name, value) -> do
          printf "%s:\t%s\n" name value

    printLastResult :: JobInfo -> IO ()
    printLastResult job = do
      let time = case jiResultTime job of
                   Nothing -> "-"
                   Just t -> show t
      let code = show (jiExitCode job)
      printf "Exit code:\t%s\nTime:\t%s\n\n" code time
      case jiStdout job of
        Nothing -> return ()
        Just text -> putStrLn $ T.unpack text

    printResult :: Database.JobResult -> IO ()
    printResult r = do
      let time = show (Database.jobResultTime r)
      let code = show (Database.jobResultExitCode r)
      printf "Exit code:\t%s\nTime:\t%s\n\n" code time
      putStrLn $ T.unpack $ Database.jobResultStdout r

updateJob :: Client ()
updateJob = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let job = object $
              toList "queue_name" (queueName command) ++
              toList "status" (status command) ++
              toList "host_name" (hostName command)
  let url = baseUrl </> "job" </> show (jobId command)
  doPut url job

deleteJob :: Client ()
deleteJob = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let url = baseUrl </> "job" </> show (jobId command)
  doDelete url

addQueue :: Client ()
addQueue = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let queue = Database.Queue {
                Database.queueName = queueObject command,
                Database.queueTitle = fromMaybe (queueObject command) (title command),
                Database.queueEnabled = fromMaybe True (enabled command),
                Database.queueScheduleName = fromMaybe "anytime" (scheduleName command),
                Database.queueHostName = hostName command
              }
  let url = baseUrl </> "queue"
  doPost url queue

updateQueue :: Client ()
updateQueue = do
    baseUrl <- getBaseUrl
    opts <- gets csCmdline
    let command = cmdCommand opts
    let queue = object $
                  toList "enabled" (enabled command) ++
                  toList "title" (title command) ++
                  toList "schedule_name" (scheduleName command) ++
                  toList "host_name" (hostName command)
    -- print queue
    let url = baseUrl </> "queue" </> queueObject command
    doPut url queue

toList _ Nothing = []
toList name (Just str) = [name .= str]

deleteQueue :: Client ()
deleteQueue = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let forceStr = if force command then "?forced=true" else ""
  let url = baseUrl </> "queue" </> queueObject command ++ forceStr
  doDelete url

doListSchedules :: Client ()
doListSchedules = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let url = baseUrl </> "schedule"
  response <- doGet url
  let check = if null (scheduleNames command)
                then const True
                else \si -> sName si `elem` scheduleNames command
  liftIO $ forM_ (response :: [ScheduleInfo]) $ \si -> do
            when (check si) $ do
              putStrLn $ sName si ++ ":"
              case sWeekdays si of
                Nothing -> putStrLn "\tany weekday"
                Just lst -> putStrLn $ "\t" ++ intercalate ", " (map show lst)
              case sTime si of
                Nothing -> putStrLn "\tany time of day"
                Just lst -> putStrLn $ "\t" ++ intercalate ", " (map show lst)

doAddSchedule :: Client ()
doAddSchedule = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  when (length (scheduleNames command) /= 1) $
    throwC $ "Exactly one schedule name must be specified when creating a schedule"
  let ts = forM (periods command) $ \str -> do
             parsePeriod str
  case ts of
    Left err -> throwC $ "Can't parse period description: " ++ show err
    Right times -> do
      let url = baseUrl </> "schedule"
      let schedule = ScheduleInfo {
                       sName = head (scheduleNames command),
                       sWeekdays = if null (weekdays command) then Nothing else Just (weekdays command),
                       sTime = if null times then Nothing else Just times
                     }
      doPost url schedule

doDeleteSchedule :: Client ()
doDeleteSchedule = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  when (length (scheduleNames command) /= 1) $
    throwC $ "Exactly one schedule name must be specified when deleting a schedule"
  let sname = head (scheduleNames command)
  let forceStr = if force command then "?forced=true" else ""
  let url = baseUrl </> "schedule" </> sname ++ forceStr
  doDelete url

doType :: Client ()
doType = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let url = baseUrl </> "type"
  response <- doGet url
  let check = if null (types command)
                then const True
                else \jt -> jtName jt `elem` types command
  liftIO $ forM_ (response :: [JobType]) $ \jt -> do
            when (check jt) $ do
              let title = fromMaybe (jtName jt) (jtTitle jt)
              putStrLn $ jtName jt ++ ":"
              putStrLn $ "\tTitle:\t" ++ title
              putStrLn $ "\tTemplate:\t" ++ jtTemplate jt
              putStrLn $ "\tOn fail:\t" ++ show (jtOnFail jt)
              putStrLn $ "\tHost:\t" ++ fromMaybe "*" (jtHostName jt)
              putStrLn $ "\tParameters:"
              forM_ (jtParams jt) $ \desc -> do
                putStrLn $ "\t* Name:\t" ++ piName desc
                putStrLn $ "\t  Type:\t" ++ show (piType desc)
                putStrLn $ "\t  Title:\t" ++ piTitle desc
                putStrLn $ "\t  Default:\t" ++ piDefault desc

doListUsers :: Client ()
doListUsers = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let url = baseUrl </> "user"
  response <- doGet url
  liftIO $ forM_ (response :: [String]) $ \name -> putStrLn name

doAddUser :: Client ()
doAddUser = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let url = baseUrl </> "user"
  pwd <- liftIO $ getPassword2
  name <- case objectUserName command of
            [n] -> return n
            _ -> fail "user name must be provided"
  let user = UserInfo name pwd
  doPost url user

doChangePassword :: Client ()
doChangePassword = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  creds <- getCredentials
  name <- case objectUserName command of
            [n] -> return n
            _ -> return $ fst creds
  let url = baseUrl </> "user" </> name
  pwd <- liftIO $ getPassword2
  let user = UserInfo name pwd
  doPut url user

doListPermissions :: Client ()
doListPermissions = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let url = baseUrl </> "user" </> grantUserName command </> "permissions"
  response <- doGet url
  liftIO $ forM_ (response :: [(Int64, Database.UserPermission)]) $ \(id, perm) -> do
              let qname = fromMaybe "*" $ Database.userPermissionQueueName perm
                  tname = fromMaybe "*" $ Database.userPermissionTypeName perm
                  host  = fromMaybe "*" $ Database.userPermissionHostName perm
              printf "Id:\t%d\nPermission:\t%s\nQueue:\t%s\nJob type:\t%s\nHost:\t%s\n"
                id (show $ Database.userPermissionPermission perm) qname tname host

doAddPermission :: Client ()
doAddPermission = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let name = grantUserName command
      url = baseUrl </> "user" </> name </> "permissions"
  perm <- case permission command of
            Just p -> return $ Database.UserPermission name p (queueName command) (typeName command) (hostName command)
            Nothing -> throwC "permission (-p) must be specified"
  doPost url perm

doRevokePermission :: Client ()
doRevokePermission = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let name = grantUserName command
  url <- case grantPermissionId command of
           Just permId -> return $ baseUrl </> "user" </> name </> "permissions" </> show permId
           Nothing -> throwC "permission ID (-i) must be specified"
  -- liftIO $ print url
  doDelete url

