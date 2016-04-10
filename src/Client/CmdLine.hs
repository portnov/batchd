{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.CmdLine where

import Data.Generics hiding (Generic)
import qualified Data.Map as M
import Data.Dates
import System.Console.CmdArgs

import Common.Types
import Client.Types

data Batch =
    Enqueue {
      managerUrl :: Maybe String,
      queueName :: Maybe String,
      typeName :: Maybe String,
      hostName :: Maybe String,
      command :: [String],
      parameters :: [String]
    }
  | List {
      managerUrl :: Maybe String,
      status :: Maybe String,
      queueToList :: [String]
    }
  | Queue {
      managerUrl :: Maybe String,
      queueMode :: CrudMode,
      queueObject :: String,
      scheduleName :: Maybe String,
      hostName :: Maybe String,
      title :: Maybe String,
      enabled :: Maybe Bool,
      force :: Bool
    }
  | Job {
      managerUrl :: Maybe String,
      jobId :: Int,
      queueName :: Maybe String,
      status :: Maybe String,
      hostName :: Maybe String,
      jobMode :: CrudMode
    }
  | Schedule {
      managerUrl :: Maybe String,
      scheduleMode :: CrudMode,
      scheduleNames :: [String],
      periods :: [String],
      weekdays :: [WeekDay],
      force :: Bool
    }
  | Type {
      managerUrl :: Maybe String,
      types :: [String]
    }
  | Stats {
      managerUrl :: Maybe String,
      queueToStat :: [String]
    }
  deriving (Show, Data, Typeable)

defaultUrl :: String
defaultUrl = "http://localhost:" ++ show defaultManagerPort

defaultQueue :: String
defaultQueue = "default"

defaultType :: String
defaultType = "command"

managerUrlAnn = Nothing &= name "url" &= typ defaultUrl &= help "batchd manager API URL"

enqueue :: Batch
enqueue = Enqueue {
    managerUrl = managerUrlAnn,
    queueName = def &= name "queue" &= typ "QUEUE" &= help "queue name",
    typeName = def &= name "type" &= typ "TYPE" &= help "job type name",
    hostName = def &= name "host" &= typ "HOST" &= help "worker host name",
    command = def &= typ "COMMAND PARAMETERS" &= args,
    parameters = def &= typ "NAME=VALUE" &= help "job parameters specified by name"
  } &= help "put a new job into queue"

list :: Batch
list = List {
    managerUrl = managerUrlAnn,
    status = def &= typ "STATUS" &= help "list only jobs of specified status",
    queueToList = def &= args &= typ "QUEUE"
  } &= help "list queues or jobs"

queue :: Batch
queue = Queue {
    managerUrl = managerUrlAnn,
    queueMode = enum [
                  Add &= help "create new queue",
                  Update &= help "modify queue",
                  Delete &= help "delete queue"],
    queueObject = defaultQueue &= argPos 0 &= typ "QUEUE",
    scheduleName = def &= typ "SCHEDULE" &= help "queue schedule name",
    hostName = Nothing &= name "host" &= typ "HOST" &= help "default host name for queue",
    title = Nothing &= name "name" &= typ "TITLE" &= help "set queue title",
    enabled = Nothing &= name "active" &= typ "TRUE" &= help "enable/disable queue",
    force = False &= help "force non-empty queue deletion"
  } &= help "create, update or delete queues"

job :: Batch
job = Job {
    managerUrl = managerUrlAnn,
    jobId = def &= typ "ID" &= argPos 0,
    status = def &= typ "STATUS" &= help "set job status",
    hostName = Nothing &= name "host" &= typ "HOST" &= help "set job host",
    queueName = def &= name "queue" &= typ "QUEUE" &= help "set job queue",
    jobMode = enum [
                Update &= help "modify job",
                Delete &= help "delete job"
              ]
  } &= help "update or delete jobs"
    
schedule :: Batch
schedule = Schedule {
    managerUrl = managerUrlAnn,
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

typesList :: Batch
typesList = Type {
    managerUrl = managerUrlAnn,
    types = [] &= typ "TYPE" &= args
  } &= name "type"
    &= help "show defined job types"

stats :: Batch
stats = Stats {
    managerUrl = managerUrlAnn,
    queueToStat = [] &= typ "QUEUE" &= args
  } &= help "print statistics on queue or on all jobs"

parseParams :: [ParamDesc] -> Batch -> JobParamInfo
parseParams desc e = 
    let posNames = map piName desc
        ordered = M.fromList $ zip posNames (command e)
        byName = M.fromList $ map parseOne (parameters e)
    in  M.union byName ordered
  where
    parseOne :: String -> (String, String)
    parseOne s = case break (== '=') s of
                   (key, (_:value)) -> (key, value)
                   (key, []) -> (key, "")

