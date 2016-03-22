{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import Data.Aeson
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import Data.Char (toLower)
import Data.Generics
import Data.Dates
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Console.CmdArgs
import System.FilePath
import Text.Printf

import CommonTypes
import qualified Database
import Schedule

data CrudMode =
    View
  | Add
  | Update
  | Delete
  deriving (Show, Data, Typeable)

data Batch =
    Enqueue {
      managerUrl :: String,
      queueName :: String,
      typeName :: String,
      hostName :: Maybe String,
      commandName :: String,
      commandParam :: String,
      parameters :: [String]
    }
  | List {
      managerUrl :: String,
      status :: Maybe String,
      queueToList :: [String]
    }
  | Queue {
      managerUrl :: String,
      queueMode :: CrudMode,
      queueName :: String,
      scheduleName :: Maybe String,
      hostName :: Maybe String,
      force :: Bool
    }
  | Schedule {
      managerUrl :: String,
      scheduleMode :: CrudMode,
      scheduleNames :: [String],
      periods :: [String],
      weekdays :: [WeekDay],
      force :: Bool
    }
  | Stats {
      managerUrl :: String,
      queueToStat :: [String]
    }
  deriving (Show, Data, Typeable)

defaultUrl :: String
defaultUrl = "http://localhost:" ++ show defaultManagerPort

defaultQueue :: String
defaultQueue = "default"

defaultType :: String
defaultType = "command"

managerUrlAnn url = url &= name "url" &= typ defaultUrl &= help "batchd manager API URL"

enqueue :: Batch
enqueue = Enqueue {
    managerUrl = managerUrlAnn defaultUrl,
    queueName = defaultQueue &= name "queue" &= typ "QUEUE" &= help "queue name",
    typeName = defaultType &= name "type" &= typ "TYPE" &= help "job type name",
    hostName = def &= name "host" &= typ "HOST" &= help "worker host name",
    commandName = defaultType &= typ "PARAMNAME" &= help "name of `command` parameter for job",
    commandParam = def &= typ "COMMAND" &= argPos 0,
    parameters = def &= typ "NAME=VALUE" &= help "job parameter"
  } &= help "put a new job into queue"

list :: Batch
list = List {
    managerUrl = managerUrlAnn defaultUrl,
    status = def &= typ "STATUS" &= help "list only jobs of specified status",
    queueToList = def &= args &= typ "QUEUE"
  } &= help "list queues or jobs"

queue :: Batch
queue = Queue {
    managerUrl = managerUrlAnn defaultUrl,
    queueMode = enum [
                  Add &= help "create new queue",
                  Update &= help "modify queue",
                  Delete &= help "delete queue"],
    queueName = defaultQueue &= argPos 0 &= typ "QUEUE",
    scheduleName = def &= typ "SCHEDULE" &= help "queue schedule name",
    hostName = Nothing &= name "host" &= typ "HOST" &= help "default host name for queue",
    force = False &= help "force non-empty queue deletion"
  } &= help "create, update or delete queues"
    
schedule :: Batch
schedule = Schedule {
    managerUrl = managerUrlAnn defaultUrl,
    scheduleMode =  enum [
                      View &= name "ls" &= help "list available schedules",
                      Add &= help "create new schedule",
                      Update &= help "modify schedule",
                      Delete &= help "delete (unused) schedule"],
    scheduleNames = def &= typ "SCHEDULE" &= args,
    periods = [] &= typ "HH:MM:SS HH:MM:SS" &= help "time of day period(s)",
    weekdays = [] &= typ "WEEKDAY" &= help "week day(s)",
    force = False &= help "delete also all queues which use this schedule and their jobs"
  } &= help "create, update or delete schedules"

stats :: Batch
stats = Stats {
    managerUrl = managerUrlAnn defaultUrl,
    queueToStat = [] &= typ "QUEUE" &= args
  } &= help "print statistics on queue or on all jobs"

parseParams :: Batch -> JobParamInfo
parseParams e = 
    let pairs = map parseOne (parameters e)
    in  M.insert (commandName e) (commandParam e) $ M.fromList pairs
  where
    parseOne :: String -> (String, String)
    parseOne s = case break (== '=') s of
                   (key, (_:value)) -> (key, value)
                   (key, []) -> (key, "")

doPut :: ToJSON a => Manager -> String -> a -> IO ()
doPut manager urlStr object = do
  url <- parseUrl urlStr
  let request = url {
                  method="PUT",
                  requestBody = RequestBodyLBS $ encode object
                }
  httpLbs request manager
  return ()

doPost :: ToJSON a => Manager -> String -> a -> IO ()
doPost manager urlStr object = do
  url <- parseUrl urlStr
  let request = url {
                  method="POST",
                  requestBody = RequestBodyLBS $ encode object
                }
  httpLbs request manager
  return ()

doDelete :: Manager -> String -> IO ()
doDelete manager urlStr = do
  url <- parseUrl urlStr
  let request = url { method="DELETE" }
  httpLbs request manager
  return ()

doGet :: FromJSON a => Manager -> String -> IO a
doGet manager urlStr = do
  url <- parseUrl urlStr
  responseLbs <- httpLbs url manager
  case eitherDecode (responseBody responseLbs) of
    Left err -> fail err
    Right res -> return res

doEnqueue :: Manager -> Batch -> IO ()
doEnqueue manager opts = do
  let qname = queueName opts

  let job = JobInfo {
      jiId = 0,
      jiQueue = qname,
      jiType = typeName opts,
      jiSeq = 0,
      jiStatus = New,
      jiTryCount = 0,
      jiHostName = hostName opts,
      jiParams = parseParams opts
    }

  let url = managerUrl opts </> "queue" </> qname
  doPut manager url job

doList :: Manager -> Batch -> IO ()
doList manager opts = do
  case queueToList opts of
    [] -> do
      let url = managerUrl opts </> "queue"
      response <- doGet manager url
      forM_ response $ \queue -> do
        printf "%s:\t%s\t%s\n"
               (Database.queueName queue)
               (Database.queueScheduleName queue)
               (fromMaybe "*" $ Database.queueHostName queue)

    qnames ->
      forM_ qnames $ \qname -> do
        statusOpt <- parseStatus (Just New) (fail "Invalid status") (status opts)
        let statusStr = case statusOpt of
                          Nothing -> "?status=all"
                          Just st -> case status opts of
                                       Nothing -> ""
                                       _ -> "?status=" ++ map toLower (show st)
        let url = managerUrl opts </> "queue" </> qname ++ statusStr
        response <- doGet manager url
        forM_ response $ \job -> do
          printf "#%d: [%d]\t%s\t%s\n" (jiId job) (jiSeq job) (jiType job) (show $ jiStatus job)
          forM_ (M.assocs $ jiParams job) $ \(name, value) -> do
            printf "\t%s:\t%s\n" name value

doStats :: Manager -> Batch -> IO ()
doStats manager opts = do
    case queueToStat opts of
      [] -> do
        let url = managerUrl opts </> "stats"
        response <- doGet manager url
        forM_ (M.assocs response) $ \rec -> do
          let qname = fst rec :: String
              stat = snd rec :: M.Map JobStatus Int
          putStrLn $ qname ++ ":"
          printStats stat
      qnames -> do
        forM_ qnames $ \qname -> do
          putStrLn $ qname ++ ":"
          let url = managerUrl opts </> "stats" </> qname
          response <- doGet manager url
          printStats response
  where
    printStats :: M.Map JobStatus Int -> IO ()
    printStats stat =
      forM_ (M.assocs stat) $ \(st, cnt) ->
          printf "\t%s:\t%d\n" (show st) cnt

addQueue :: Manager -> Batch -> IO ()
addQueue manager opts = do
  let queue = Database.Queue {
                Database.queueName = queueName opts,
                Database.queueScheduleName = fromMaybe "anytime" (scheduleName opts),
                Database.queueHostName = hostName opts
              }
  let url = managerUrl opts </> "queue"
  doPut manager url queue

updateQueue :: Manager -> Batch -> IO ()
updateQueue manager opts = do
    let queue = object $
                  toList "schedule_name" (scheduleName opts) ++
                  toList "host_name" (hostName opts)
    let url = managerUrl opts </> "queue" </> queueName opts
    doPost manager url queue
  where
    toList _ Nothing = []
    toList name (Just str) = [name .= str]

deleteQueue :: Manager -> Batch -> IO ()
deleteQueue manager opts = do
  let forceStr = if force opts then "?forced=true" else ""
  let url = managerUrl opts </> "queue" </> queueName opts ++ forceStr
  doDelete manager url

doListSchedules :: Manager -> Batch -> IO ()
doListSchedules manager opts = do
  let url = managerUrl opts </> "schedule"
  response <- doGet manager url
  let check = if null (scheduleNames opts)
                then const True
                else \si -> sName si `elem` scheduleNames opts
  forM_ response $ \si -> do
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
  when (length (scheduleNames opts) /= 1) $
    fail $ "Exactly one schedule name must be specified when creating a schedule"
  let ts = forM (periods opts) $ \str -> do
             parsePeriod str
  case ts of
    Left err -> fail $ "Can't parse period description: " ++ show err
    Right times -> do
      let url = managerUrl opts </> "schedule"
      let schedule = ScheduleInfo {
                       sName = head (scheduleNames opts),
                       sWeekdays = if null (weekdays opts) then Nothing else Just (weekdays opts),
                       sTime = if null times then Nothing else Just times
                     }
      doPut manager url schedule

main :: IO ()
main = do
  let mode = cmdArgsMode $ modes [enqueue, list &= name "ls", queue, schedule, stats]
  opts <- cmdArgsRun mode

  manager <- newManager defaultManagerSettings

  case opts of
    Enqueue {} -> doEnqueue manager opts
    List {} -> doList manager opts
    Stats {} -> doStats manager opts
    Queue {} ->
      case queueMode opts of
        Add -> addQueue manager opts
        Update -> updateQueue manager opts
        Delete -> deleteQueue manager opts
    Schedule {} -> 
      case scheduleMode opts of
        View -> doListSchedules manager opts
        Add -> doAddSchedule manager opts

