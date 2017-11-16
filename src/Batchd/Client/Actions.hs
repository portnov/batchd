{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module contains definitions of client actions.
-- These functions prepare REST request structure, execute HTTP request
-- and show response in human-readable form to stdout.
module Batchd.Client.Actions where

import Control.Monad
import Control.Monad.State
import Data.Int
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy
import Data.Char
import Data.List (intercalate, transpose)
import Data.Aeson
import System.FilePath
import System.Exit
import Text.Printf
import Text.PrettyPrint.Boxes

import Batchd.Core.Common.Localize
import qualified Batchd.Common.Data as Database
import Batchd.Common.Types
import Batchd.Common.Schedule
import Batchd.Common.Config
import Batchd.Client.CmdLine
import Batchd.Client.Config
import Batchd.Client.Http
import Batchd.Client.Monad

localTimeToUTC' :: Maybe (Maybe LocalTime) -> Client (Maybe (Maybe UTCTime))
localTimeToUTC' Nothing = return Nothing
localTimeToUTC' (Just Nothing) = return (Just Nothing)
localTimeToUTC' (Just (Just local)) = do
    tz <- liftIO $ getCurrentTimeZone
    return $ Just $ Just $ localTimeToUTC tz local

-- | Put job to the queue.
doEnqueue :: Client ()
doEnqueue = do
  cfg <- gets csConfig
  baseUrl <- getBaseUrl
  creds <- getCredentials
  opts <- gets csCmdline
  t <- liftIO $ getTypeName opts cfg
  jtr <- liftIO $ loadTemplate t
  case jtr of
    Left err -> throwC =<< (__f "Can't load job type description: {}" (Single $ Shown err))
    Right jtype -> do
      qname <- liftIO $ getQueueName opts cfg
      host <- liftIO $ getHostName opts cfg
      jobStartTime <- join `fmap` localTimeToUTC' (startTime $ cmdCommand opts)

      let job = JobInfo {
          jiId = 0,
          jiQueue = qname,
          jiType = t,
          jiSeq = 0,
          jiUserName = fst creds,
          jiCreateTime = zeroUtcTime,
          jiStartTime = jobStartTime,
          jiStatus = New,
          jiTryCount = 0,
          jiHostName = host,
          jiNotes = jobNotes (cmdCommand opts),
          jiResultTime = Nothing,
          jiExitCode = Nothing,
          jiStdout = Nothing,
          jiStderr = Nothing,
          jiParams = parseParams (jtParams jtype) (cmdCommand opts)
        }
      -- debug (__ "Job to be queued: {}") (Single $ show job)

      let url = baseUrl </> "queue" </> qname
      doPost url job

mkTable :: [[String]] -> Box
mkTable table = hsep 1 top [vcat left (map text column) | column <- table]

printTable :: Int -> [[String]] -> IO ()
printTable indent table =
    printBox $ emptyBox 0 indent <> mkTable table

-- | List queues or jobs.
doList :: Client ()
doList = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  case queueToList command of
    [] -> do
      let url = baseUrl </> "queue"
      response <- doGet url
      let queuesTable = flip map (response :: [Database.Queue]) $ \queue ->
                           [(if Database.queueEnabled queue then "[*]" else "[ ]") :: String,
                             Database.queueName queue,
                             Database.queueTitle queue,
                             Database.queueScheduleName queue,
                             fromMaybe "*" $ Database.queueHostName queue,
                             maybe "no" show $ Database.queueAutostartJobCount queue]
                        
      liftIO $ printTable 0 $ transpose queuesTable

    qnames ->
      forM_ qnames $ \qname -> do
        statusOpt <- parseStatus (Just New) (throwC =<< (__ "Invalid status")) (status command)
        let statusStr = case statusOpt of
                          Nothing -> "?status=all"
                          Just st -> case status command of
                                       Nothing -> ""
                                       _ -> "?status=" ++ map toLower (show st)
        let url = baseUrl </> "queue" </> qname </> "jobs" ++ statusStr
        response <- doGet url

        liftIO $ forM_ (response :: [JobInfo]) $ \job -> do
                  printf "#%d: [%d]\t%s\t%s\t%s\n" (jiId job) (jiSeq job) (jiType job) (show $ jiStatus job) (fromMaybe "" $ jiNotes job)
                  forM_ (M.assocs $ jiParams job) $ \(name, value) -> do
                    printf "\t%s:\t%s\n" name value

-- | Retrieve statistics
doStats :: Client ()
doStats = do
    baseUrl <- getBaseUrl
    opts <- gets csCmdline
    let command = cmdCommand opts
    case queueToStat command of
      [] -> do
        let url = baseUrl </> "stats"
        response <- doGet url
        liftIO $ forM_ (M.assocs response) $ \record -> do
                  let qname = fst record :: String
                      stats = snd record
                      
                  liftIO $ putStrLn $ qname ++ ":"
                  liftIO $ printStats stats
      qnames -> do
        forM_ qnames $ \qname -> do
          liftIO $ putStrLn $ qname ++ ":"
          let url = baseUrl </> "stats" </> qname
          response <- doGet url
          liftIO $ printStats response
  where
    printStats :: ByStatus Int -> IO ()
    printStats (ByStatus stat) = do
      printTable 4 $ transpose $ [[show status ++ ":", show count] | (status, count) <- M.assocs stat]

printField :: Formatable value => TL.Text -> IO TL.Text -> value -> IO ()
printField sep ioName value = do
  name <- ioName
  TLIO.putStrLn $ format "{}{}:\t{}" (sep, name, value)

translateTable :: [(IO TL.Text, String)] -> IO [[String]]
translateTable pairs =
  forM pairs $ \(ioTitle, value) -> do
    title <- ioTitle
    return [TL.unpack title ++ ":", value]

-- | View job details or results.
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
               let showEc Nothing = "-"
                   showEc (Just ExitSuccess) = "0"
                   showEc (Just (ExitFailure n)) = show n
               let responseTable = flip map (response :: [Database.JobResult]) $ \result ->
                                     [show (Database.jobResultTime result),
                                      showEc (Database.jobResultExitCode result),
                                      T.unpack (Database.jobResultStdout result)
                                     ]
               liftIO $ printTable 0 $ transpose responseTable
  where
    printJob :: JobInfo -> IO ()
    printJob job = do
      let host = fromMaybe "*" $ jiHostName job
      table <- translateTable $ [
                  ((__ "Order"),  show $ jiSeq job),
                  ((__ "Type"),  jiType job),
                  ((__ "Queue"),  jiQueue job),
                  ((__ "Host"), host),
                  ((__ "Notes"), fromMaybe "-" $ jiNotes job),
                  ((__ "User"),  jiUserName job),
                  ((__ "Created"),  show $ jiCreateTime job),
                  ((__ "Start"),  maybe "*" show $ jiStartTime job),
                  ((__ "Status"),  show $ jiStatus job),
                  ((__ "Try count"),  show $ jiTryCount job)
                ]
      printTable 0 $ transpose table
      let params = [[name ++ ":", value] | (name, value) <- M.assocs (jiParams job)]
      printTable 4 $ transpose params

    printLastResult :: JobInfo -> IO ()
    printLastResult job = do
      let time = case jiResultTime job of
                   Nothing -> "-"
                   Just t -> show t
      let code = show (jiExitCode job)
      TLIO.putStrLn =<< (__f "Exit code:\t{}\nTime:\t{}\n\n" (code, time))
      case jiStdout job of
        Nothing -> return ()
        Just text -> putStrLn $ T.unpack text

    printResult :: Database.JobResult -> IO ()
    printResult r = do
      let time = show (Database.jobResultTime r)
      let code = show (Database.jobResultExitCode r)
      TLIO.putStrLn =<< (__f "Exit code:\t{}\nTime:\t{}\n\n" (code, time))
      putStrLn $ T.unpack $ Database.jobResultStdout r

-- checkIncompatibleOptions :: [(String, Bool)] -> Client ()
-- checkIncompatibleOptions opts =
--     when (length (filter snd opts) > 1) $ do
--       throwC $ printf "Only one option of %s can be specified at once" enabled
--   where
--     enabled = intercalate ", " $ map fst $ filter snd opts

checkModes :: [Client (Maybe a)] -> Client a
checkModes list = go Nothing list
  where
    go Nothing [] = throwC =<< (__ "No mode is selected")
    go (Just m) [] = return m
    go Nothing (m:ms) = do
      result <- m
      go result ms
    go selected@(Just _) (m:ms) = do
      result <- m
      case result of
        Nothing -> go selected ms
        Just _ -> throwC =<< (__ "Only one mode can be selected")

-- | Update job.
updateJob :: Client ()
updateJob = do
    baseUrl <- getBaseUrl
    opts <- gets csCmdline
    let command = cmdCommand opts
    job <- checkModes [mMove command, mPrioritize command, mUpdate command]
    let url = baseUrl </> "job" </> show (jobId command)
    doPut url job

  where
    mMove command = do
      case queueName command of
        Just qname -> return $ Just $ object ["move" .= qname]
        Nothing -> return Nothing

    mPrioritize command = do
      case prioritize command of
        Just action -> return $ Just $ object ["priority" .= action]
        Nothing -> return Nothing

    mUpdate command = do
      if isNothing (queueName command) && isNothing (prioritize command)
        then do
             jobStartTime <- localTimeToUTC' (startTime command)
             return $ Just $ object $
                                toList "status" (status command) ++
                                toList "host_name" (hostName command) ++
                                toList "notes" (jobNotes command) ++
                                toList "start_time" jobStartTime
        else return Nothing

-- | Delete job.
deleteJob :: Client ()
deleteJob = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let url = baseUrl </> "job" </> show (jobId command)
  doDelete url

-- | Create queue.
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
                Database.queueHostName = hostName command,
                Database.queueAutostartJobCount = join (autostartCount command)
              }
  let url = baseUrl </> "queue"
  doPost url queue

-- | Update queue.
updateQueue :: Client ()
updateQueue = do
    baseUrl <- getBaseUrl
    opts <- gets csCmdline
    let command = cmdCommand opts
    let queue = object $
                  toList "enabled" (enabled command) ++
                  toList "title" (title command) ++
                  toList "schedule_name" (scheduleName command) ++
                  toList "host_name" (hostName command) ++ 
                  toList "autostart_job_count" (autostartCount command)
    -- print queue
    let url = baseUrl </> "queue" </> queueObject command
    doPut url queue

toList :: (ToJSON v, KeyValue t) => T.Text -> Maybe v -> [t]
toList _ Nothing = []
toList name (Just str) = [name .= str]

-- | Delete queue.
deleteQueue :: Client ()
deleteQueue = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let forceStr = if force command then "?forced=true" else ""
  let url = baseUrl </> "queue" </> queueObject command ++ forceStr
  doDelete url

-- | List schedules
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

-- | Create a schedule.
doAddSchedule :: Client ()
doAddSchedule = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  when (length (scheduleNames command) /= 1) $
    throwC =<< (__ "Exactly one schedule name must be specified when creating a schedule")
  let ts = forM (periods command) $ \str -> do
             parsePeriod str
  case ts of
    Left err -> throwC =<< (__f "Can't parse period description: {}" (Single $ show err))
    Right times -> do
      let url = baseUrl </> "schedule"
      let schedule = ScheduleInfo {
                       sName = head (scheduleNames command),
                       sWeekdays = if null (weekdays command) then Nothing else Just (weekdays command),
                       sTime = if null times then Nothing else Just times
                     }
      doPost url schedule

-- | Delete schedule.
doDeleteSchedule :: Client ()
doDeleteSchedule = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  when (length (scheduleNames command) /= 1) $
    throwC =<< (__ "Exactly one schedule name must be specified when deleting a schedule")
  let sname = head (scheduleNames command)
  let forceStr = if force command then "?forced=true" else ""
  let url = baseUrl </> "schedule" </> sname ++ forceStr
  doDelete url

-- | List job types.
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
              table <- translateTable $ [
                              (__ "Title", title),
                              (__ "Template", jtTemplate jt),
                              (__ "On fail", show (jtOnFail jt)),
                              (__ "Host", fromMaybe "*" (jtHostName jt))
                            ]
              printTable 4 $ transpose table
              paramsLine <- (__ "Parameters:")
              TLIO.putStrLn $ "    " `TL.append` paramsLine
              forM_ (jtParams jt) $ \desc -> do
                paramsTable <- translateTable $ [
                                  (__ "Name", piName desc),
                                  (__ "Type", show (piType desc)),
                                  (__ "Title",  piTitle desc),
                                  (__ "Default",  piDefault desc)
                                ]
                let box = emptyBox 0 4 <> char '*' <+> mkTable (transpose paramsTable)
                printBox box

doMonitor :: Client ()
doMonitor = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let monitor = baseUrl </> "monitor"
  url <- case metricTime command of
           LastSample -> case metricPrefix command of
                           Nothing -> fail "metric name must be specified for --last mode"
                           Just name -> return $ monitor </> name </> "last"
           CurrentSample -> 
             let byPrefix = case metricPrefix command of
                              Nothing -> monitor </> "current"
                              Just prefix -> monitor </> prefix </> "current"
             in return $ case metricView command of
                          Plain -> byPrefix </> "plain"
                          Tree  -> byPrefix </> "tree"
  response <- doGet url
--   liftIO $ putStrLn $ show (response :: Value)
  liftIO $ forM_ (response :: [Database.MetricRecord]) $ \r -> do
      TIO.putStr $ Database.metricRecordName r `T.append` ": "
      case Database.metricRecordKind r of
        Counter -> TLIO.putStrLn =<< (__f "count: {value}" r)
        Gauge -> TLIO.putStrLn =<< (__f "value: {value}" r)
        Label -> TLIO.putStrLn =<< (__f "text: {text}" r)
        Distribution -> do
          TLIO.putStrLn =<< (__f "mean: {mean}; variance: {variance}; count: {count}; sum: {sum}; min: {min}; max: {max}" r)

-- | List users.
doListUsers :: Client ()
doListUsers = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let url = baseUrl </> "user"
  response <- doGet url
  liftIO $ forM_ (response :: [String]) $ \name -> putStrLn name

-- | Create user
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

-- | Change user password
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

-- | List user permissions
doListPermissions :: Client ()
doListPermissions = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let url = baseUrl </> "user" </> grantUserName command </> "permissions"
  response <- doGet url
  liftIO $ forM_ (response :: [(Int64, Database.UserPermission)]) $ \(id, perm) -> do
              table <- translateTable $ [
                           (__ "ID", show id),
                           (__ "Permission", show $ Database.userPermissionPermission perm),
                           (__ "Queue", fromMaybe "*" $ Database.userPermissionQueueName perm),
                           (__ "Job type", fromMaybe "*" $ Database.userPermissionTypeName perm),
                           (__ "Host", fromMaybe "*" $ Database.userPermissionHostName perm)
                        ]
              printTable 0 $ transpose table
              putStrLn ""

-- | Add user permission
doAddPermission :: Client ()
doAddPermission = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let name = grantUserName command
      url = baseUrl </> "user" </> name </> "permissions"
  perm <- case permission command of
            Just p -> return $ Database.UserPermission name p (queueName command) (typeName command) (hostName command)
            Nothing -> throwC =<< (__ "permission (-p) must be specified")
  doPost url perm

-- | Revoke user permission.
doRevokePermission :: Client ()
doRevokePermission = do
  baseUrl <- getBaseUrl
  opts <- gets csCmdline
  let command = cmdCommand opts
  let name = grantUserName command
  url <- case grantPermissionId command of
           Just permId -> return $ baseUrl </> "user" </> name </> "permissions" </> show permId
           Nothing -> throwC =<< (__ "permission ID (-i) must be specified")
  -- liftIO $ print url
  doDelete url

