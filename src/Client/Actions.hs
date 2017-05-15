{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module Client.Actions where

import Control.Exception
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Char
import Data.List (intercalate)
import Data.Aeson
import Network.HTTP.Client
import System.FilePath
import Text.Printf

import Common.Types
import qualified Common.Data as Database
import Common.Schedule
import Common.Config
import Client.Types
import Client.CmdLine
import Client.Config
import Client.Http

getCredentials :: Batch -> IO Credentials
getCredentials opts = do
  cfg <- loadClientConfig
  name <- getUserName (username opts) cfg
  mbPassword <- getConfigParam' (password opts) "BATCH_PASSWORD" (ccPassword cfg)
  pass <- case mbPassword of
            Just p -> return p
            Nothing -> getPassword "Password: "
  return (name, pass)

doEnqueue :: Manager -> Batch -> IO ()
doEnqueue manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  t <- getTypeName (typeName opts) cfg
  jtr <- loadTemplate t
  case jtr of
    Left err -> throw $ ClientException $ show err
    Right jtype -> do
      qname <- getQueueName (queueName opts) cfg
      host <- getHostName (hostName opts) cfg

      let job = JobInfo {
          jiId = 0,
          jiQueue = qname,
          jiType = t,
          jiSeq = 0,
          jiCreateTime = zeroUtcTime,
          jiStatus = New,
          jiTryCount = 0,
          jiHostName = host,
          jiResultTime = Nothing,
          jiExitCode = Nothing,
          jiStdout = Nothing,
          jiStderr = Nothing,
          jiParams = parseParams (jtParams jtype) opts
        }

      let url = baseUrl </> "queue" </> qname
      doPut manager creds url job

doList :: Manager -> Batch -> IO ()
doList manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  case queueToList opts of
    [] -> do
      let url = baseUrl </> "queue"
      response <- doGet manager creds url
      forM_ (response :: [Database.Queue]) $ \queue -> do
        printf "[%s]\t%s:\t%s\t%s\t%s\n"
               ((if Database.queueEnabled queue then "*" else " ") :: String)
               (Database.queueName queue)
               (Database.queueTitle queue)
               (Database.queueScheduleName queue)
               (fromMaybe "*" $ Database.queueHostName queue)

    qnames ->
      forM_ qnames $ \qname -> do
        statusOpt <- parseStatus (Just New) (throw $ ClientException "Invalid status") (status opts)
        let statusStr = case statusOpt of
                          Nothing -> "?status=all"
                          Just st -> case status opts of
                                       Nothing -> ""
                                       _ -> "?status=" ++ map toLower (show st)
        let url = baseUrl </> "queue" </> qname ++ statusStr
        response <- doGet manager creds url
        forM_ (response :: [JobInfo]) $ \job -> do
          printf "#%d: [%d]\t%s\t%s\n" (jiId job) (jiSeq job) (jiType job) (show $ jiStatus job)
          forM_ (M.assocs $ jiParams job) $ \(name, value) -> do
            printf "\t%s:\t%s\n" name value

doStats :: Manager -> Batch -> IO ()
doStats manager opts = do
    cfg <- loadClientConfig
    baseUrl <- getManagerUrl (managerUrl opts) cfg
    creds <- getCredentials opts
    case queueToStat opts of
      [] -> do
        let url = baseUrl </> "stats"
        response <- doGet manager creds url
        forM_ (M.assocs response) $ \rec -> do
          let qname = fst rec :: String
              stat = snd rec :: M.Map JobStatus Int
          putStrLn $ qname ++ ":"
          printStats stat
      qnames -> do
        forM_ qnames $ \qname -> do
          putStrLn $ qname ++ ":"
          let url = baseUrl </> "stats" </> qname
          response <- doGet manager creds url
          printStats response
  where
    printStats :: M.Map JobStatus Int -> IO ()
    printStats stat =
      forM_ (M.assocs stat) $ \(st, cnt) ->
          printf "\t%s:\t%d\n" (show st) cnt

viewJob :: Manager -> Batch -> IO ()
viewJob manager opts = do
    cfg <- loadClientConfig
    baseUrl <- getManagerUrl (managerUrl opts) cfg
    creds <- getCredentials opts
    let showDescription = if viewDescription opts
                            then True
                            else if not (viewResult opts) && not (viewAll opts)
                                   then True
                                   else viewDescription opts
    if (showDescription || viewResult opts) && not (viewAll opts)
      then do
           let url = baseUrl </> "job" </> show (jobId opts)
           response <- doGet manager creds url
           when (showDescription) $
               printJob response
           when (viewResult opts) $
               printLastResult response
      else do
           when (showDescription) $ do
               let url = baseUrl </> "job" </> show (jobId opts)
               response <- doGet manager creds url
               printJob response
           when (viewAll opts) $ do
               let url = baseUrl </> "job" </> show (jobId opts) </> "results"
               response <- doGet manager creds url
               forM_ (response :: [Database.JobResult]) $ \result ->
                   printResult result
  where
    printJob :: JobInfo -> IO ()
    printJob job = do
      let host = fromMaybe "*" $ jiHostName job
      printf "Order:\t%d\nType:\t%s\nQueue:\t%s\nHost:\t%s\nCreated:\t%s\nStatus:\t%s\nTry count:\t%d\n"
        (jiSeq job) (jiType job) (jiQueue job) host (show $ jiCreateTime job)
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

updateJob :: Manager -> Batch -> IO ()
updateJob manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let job = object $
              toList "queue_name" (queueName opts) ++
              toList "status" (status opts) ++
              toList "host_name" (hostName opts)
  let url = baseUrl </> "job" </> show (jobId opts)
  doPost manager creds url job

deleteJob :: Manager -> Batch -> IO ()
deleteJob manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let url = baseUrl </> "job" </> show (jobId opts)
  doDelete manager creds url

addQueue :: Manager -> Batch -> IO ()
addQueue manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let queue = Database.Queue {
                Database.queueName = queueObject opts,
                Database.queueTitle = fromMaybe (queueObject opts) (title opts),
                Database.queueEnabled = fromMaybe True (enabled opts),
                Database.queueScheduleName = fromMaybe "anytime" (scheduleName opts),
                Database.queueHostName = hostName opts
              }
  let url = baseUrl </> "queue"
  doPut manager creds url queue

updateQueue :: Manager -> Batch -> IO ()
updateQueue manager opts = do
    cfg <- loadClientConfig
    baseUrl <- getManagerUrl (managerUrl opts) cfg
    creds <- getCredentials opts
    let queue = object $
                  toList "enabled" (enabled opts) ++
                  toList "title" (title opts) ++
                  toList "schedule_name" (scheduleName opts) ++
                  toList "host_name" (hostName opts)
    -- print queue
    let url = baseUrl </> "queue" </> queueObject opts
    doPost manager creds url queue

toList _ Nothing = []
toList name (Just str) = [name .= str]

deleteQueue :: Manager -> Batch -> IO ()
deleteQueue manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let forceStr = if force opts then "?forced=true" else ""
  let url = baseUrl </> "queue" </> queueObject opts ++ forceStr
  doDelete manager creds url

doListSchedules :: Manager -> Batch -> IO ()
doListSchedules manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let url = baseUrl </> "schedule"
  response <- doGet manager creds url
  let check = if null (scheduleNames opts)
                then const True
                else \si -> sName si `elem` scheduleNames opts
  forM_ (response :: [ScheduleInfo]) $ \si -> do
    when (check si) $ do
      putStrLn $ sName si ++ ":"
      case sWeekdays si of
        Nothing -> putStrLn "\tany weekday"
        Just lst -> putStrLn $ "\t" ++ intercalate ", " (map show lst)
      case sTime si of
        Nothing -> putStrLn "\tany time of day"
        Just lst -> putStrLn $ "\t" ++ intercalate ", " (map show lst)

doAddSchedule :: Manager -> Batch -> IO ()
doAddSchedule manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  when (length (scheduleNames opts) /= 1) $
    throw $ ClientException $ "Exactly one schedule name must be specified when creating a schedule"
  let ts = forM (periods opts) $ \str -> do
             parsePeriod str
  case ts of
    Left err -> throw $ ClientException $ "Can't parse period description: " ++ show err
    Right times -> do
      let url = baseUrl </> "schedule"
      let schedule = ScheduleInfo {
                       sName = head (scheduleNames opts),
                       sWeekdays = if null (weekdays opts) then Nothing else Just (weekdays opts),
                       sTime = if null times then Nothing else Just times
                     }
      doPut manager creds url schedule

doDeleteSchedule :: Manager -> Batch -> IO ()
doDeleteSchedule manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  when (length (scheduleNames opts) /= 1) $
    throw $ ClientException $ "Exactly one schedule name must be specified when deleting a schedule"
  let sname = head (scheduleNames opts)
  let forceStr = if force opts then "?forced=true" else ""
  let url = baseUrl </> "schedule" </> sname ++ forceStr
  doDelete manager creds url

doType :: Manager -> Batch -> IO ()
doType manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let url = baseUrl </> "type"
  response <- doGet manager creds url
  let check = if null (types opts)
                then const True
                else \jt -> jtName jt `elem` types opts
  forM_ (response :: [JobType]) $ \jt -> do
    when (check jt) $ do
      putStrLn $ jtName jt ++ ":"
      putStrLn $ "\tTemplate:\t" ++ jtTemplate jt
      putStrLn $ "\tOn fail:\t" ++ show (jtOnFail jt)
      putStrLn $ "\tHost:\t" ++ fromMaybe "*" (jtHostName jt)
      putStrLn $ "\tParameters:"
      forM_ (jtParams jt) $ \desc -> do
        putStrLn $ "\t* Name:\t" ++ piName desc
        putStrLn $ "\t  Type:\t" ++ show (piType desc)
        putStrLn $ "\t  Title:\t" ++ piTitle desc
        putStrLn $ "\t  Default:\t" ++ piDefault desc

doListUsers :: Manager -> Batch -> IO ()
doListUsers manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let url = baseUrl </> "user"
  response <- doGet manager creds url
  forM_ (response :: [String]) $ \name -> putStrLn name

doAddUser :: Manager -> Batch -> IO ()
doAddUser manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let url = baseUrl </> "user"
  pwd <- getPassword2
  name <- case objectUserName opts of
            [n] -> return n
            _ -> fail "user name must be provided"
  let user = UserInfo name pwd
  doPut manager creds url user

doListPermissions :: Manager -> Batch -> IO ()
doListPermissions manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let url = baseUrl </> "user" </> grantUserName opts </> "permissions"
  response <- doGet manager creds url
  forM_ (response :: [Database.UserPermission]) $ \perm -> do
      let qname = fromMaybe "*" $ Database.userPermissionQueueName perm
      printf "Permission:\t%s\nQueue:\t%s\n\n"
        (show $ Database.userPermissionPermission perm) qname

doAddPermission :: Manager -> Batch -> IO ()
doAddPermission manager opts = do
  cfg <- loadClientConfig
  baseUrl <- getManagerUrl (managerUrl opts) cfg
  creds <- getCredentials opts
  let name = grantUserName opts
      url = baseUrl </> "user" </> name </> "permissions"
      perm = Database.UserPermission name (permission opts) (queueName opts)
  doPut manager creds url perm

