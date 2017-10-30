{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


module Batchd.Client.Actions where

import Control.Monad
import Control.Monad.State
import Data.Int
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy
import Data.Char
import Data.List (intercalate)
import Data.Aeson
import System.FilePath
import Text.Printf

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize
import qualified Batchd.Common.Data as Database
import Batchd.Common.Schedule
import Batchd.Core.Common.Config
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
          jiResultTime = Nothing,
          jiExitCode = Nothing,
          jiStdout = Nothing,
          jiStderr = Nothing,
          jiParams = parseParams (jtParams jtype) (cmdCommand opts)
        }
      -- debug (__ "Job to be queued: {}") (Single $ show job)

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
        statusOpt <- parseStatus (Just New) (throwC =<< (__ "Invalid status")) (status command)
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

printField :: Formatable value => TL.Text -> IO TL.Text -> value -> IO ()
printField sep ioName value = do
  name <- ioName
  TLIO.putStrLn $ format "{}{}:\t{}" (sep, name, value)

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
      printField "" (__ "Order") (jiSeq job)
      printField "" (__ "Type") (jiType job)
      printField "" (__ "Queue") (jiQueue job)
      printField "" (__ "Host") host
      printField "" (__ "User") (jiUserName job)
      printField "" (__ "Created") (show $ jiCreateTime job)
      printField "" (__ "Start") (maybe "*" show $ jiStartTime job)
      printField "" (__ "Status") (show $ jiStatus job)
      printField "" (__ "Try count") (jiTryCount job)
      forM_ (M.assocs $ jiParams job) $ \(name, value) -> do
          printf "%s:\t%s\n" name value

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
                                toList "start_time" jobStartTime
        else return Nothing

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
              printField "\t" (__ "Title") title
              printField "\t" (__ "Template") (jtTemplate jt)
              printField "\t" (__ "On fail") (Shown (jtOnFail jt))
              printField "\t" (__ "Host") (fromMaybe "*" (jtHostName jt))
              TLIO.putStrLn =<< (__ "\tParameters:")
              forM_ (jtParams jt) $ \desc -> do
                printField "\t* " (__ "Name") (piName desc)
                printField "\t  " (__ "Type") (Shown (piType desc))
                printField "\t  " (__ "Title") (piTitle desc)
                printField "\t  " (__ "Default") (piDefault desc)

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
              printf "Id:\t%d\nPermission:\t%s\nQueue:\t%s\nJob type:\t%s\nHost:\t%s\n\n"
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
            Nothing -> throwC =<< (__ "permission (-p) must be specified")
  doPost url perm

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

