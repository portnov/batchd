{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.CmdLine where

import Control.Monad
import Data.Generics hiding (Generic)
import qualified Data.Map as M
import Data.Int
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Dates
import Data.Char (toLower)
import Data.Semigroup ((<>))
import Options.Applicative
import System.Log.Heavy
import qualified Text.Parsec as Parsec 

import System.IO.Unsafe (unsafePerformIO)

import Common.Types
import Common.Data (MoveAction (..))
import Client.Types

data CmdLine = CmdLine {
    cmdCommon :: CommonOpts,
    cmdCommand :: Command
  } deriving (Eq, Show)

data CommonOpts = CommonOpts {
      managerUrl :: Maybe String,
      username :: Maybe String,
      password :: Maybe String,
      logLevel :: Level
    }
  deriving (Eq, Show)
    

data Command =
    Enqueue {
      queueName :: Maybe String,
      typeName :: Maybe String,
      hostName :: Maybe String,
      startTime :: Maybe (Maybe LocalTime),
      jobCommand :: [String],
      parameters :: [String]
    }
  | List {
      status :: Maybe String,
      queueToList :: [String]
    }
  | Queue {
      queueMode :: CrudMode,
      queueObject :: String,
      scheduleName :: Maybe String,
      hostName :: Maybe String,
      title :: Maybe String,
      enabled :: Maybe Bool,
      force :: Bool
    }
  | Job {
      jobId :: Int,
      queueName :: Maybe String,
      prioritize :: Maybe MoveAction,
      startTime :: Maybe (Maybe LocalTime),
      status :: Maybe String,
      hostName :: Maybe String,
      viewDescription :: Bool,
      viewResult :: Bool,
      viewAll :: Bool,
      jobMode :: CrudMode
    }
  | Schedule {
      scheduleMode :: CrudMode,
      scheduleNames :: [String],
      periods :: [String],
      weekdays :: [WeekDay],
      force :: Bool
    }
  | User {
      userMode :: CrudMode,
      objectUserName :: [String]
    }
  | Grant {
      grantMode :: CrudMode,
      grantUserName :: String,
      grantPermissionId :: Maybe Int64,
      permission :: Maybe Permission,
      queueName :: Maybe String,
      typeName :: Maybe String,
      hostName :: Maybe String
    }
  | Type {
      types :: [String]
    }
  | Stats {
      queueToStat :: [String]
    }
  | Shell
  deriving (Eq, Show, Data, Typeable)

defaultUrl :: String
defaultUrl = "http://localhost:" ++ show defaultManagerPort

defaultQueue :: String
defaultQueue = "default"

defaultType :: String
defaultType = "command"

-- managerUrlAnn = Nothing &= name "url" &= typ defaultUrl &= help "batchd manager API URL"

-- usernameAnn = Nothing &= name "user" &= typ "USER" &= help "batchd user name"

-- passwordAnn = Nothing &= name "password" &= typ "PASSWORD" &= help "batchd user password"

parser :: Parser CmdLine
parser = CmdLine <$> commonOpts <*> commands

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
    <$> optional (strOption
        ( long "manager-url"
        <> short 'M'
        <> metavar "URL"
        <> help "batchd manager API URL"))
    <*> optional (strOption
        ( long "username"
        <> short 'U'
        <> metavar "NAME"
        <> help "batchd user name"))
    <*> optional (strOption
        ( long "password"
        <> short 'P'
        <> metavar "PASSWORD"
        <> help "batchd user password"))
    <*> verbosity

verbosity :: Parser Level
verbosity =
      flag' debug_level (short 'd' <> long "debug" <> help "enable client debug output")
  <|> flag' verbose_level (short 'v' <> long "verbose" <> help "be verbose")
  <|> flag info_level disable_logging (short 'q' <> long "quiet" <> help "be quiet")

required :: Read a => String -> Char -> String -> String -> Parser a
required longName shortName meta helpText =
  option auto (long longName <> short shortName <> metavar meta <> help helpText)

optionalF :: Read a => String -> Char -> String -> String -> Parser (Maybe a)
optionalF longName shortName meta helpText =
  optional $ required longName shortName meta helpText

requiredString :: String -> Char -> String -> String -> Parser String
requiredString longName shortName meta helpText =
  strOption (long longName <> short shortName <> metavar meta <> help helpText)

optionalString :: String -> Char -> String -> String -> Parser (Maybe String)
optionalString longName shortName meta helpText =
  optional $ requiredString longName shortName meta helpText

enqueue :: Parser Command
enqueue = Enqueue
  <$> optionalString "queue" 'q' "QUEUE" "queue name"
  <*> optionalString "type"  't' "TYPE"  "job type name"
  <*> optionalString "host"  'h' "HOST"  "worker host name"
  <*> (optional $ option timeReader (long "at" <> long "start" <> metavar "YYYY-MM-DD HH:MM:SS" <> help "set job start time"))
  <*> many (strArgument  (metavar "COMMAND"))
  <*> many (requiredString "parameter" 'p' "NAME=VALUE" "job parameters specified by name")

list :: Parser Command
list = List
  <$> (optionalString "status" 's' "STATUS" "list only jobs of specified status"
       <|> flag Nothing (Just "all") (long "all" <> short 'a' <> help "synonym for --status=all"))
  <*> many (strArgument (metavar "QUEUE" <> help "queue to list"))

crudMode :: [(CrudMode, String)] -> Parser CrudMode
crudMode [] = error "crudMode called with empty list"
crudMode modes@((dflt,_):_) = foldr1 (<|>) $ map go modes
  where
    go (mode, helpText) =
      let (longName, shortName) = case mode of
                                    View -> ("view", 'v')
                                    Add -> ("add", 'a')
                                    Update -> ("update", 'u')
                                    Delete -> ("delete", 'd')
      in flag dflt mode
          ( long longName 
          <> short shortName
          <> help helpText )

priorityReader :: ReadM MoveAction
priorityReader = maybeReader (check . map toLower)
  where
    check s
      | s `elem` ["first", "f"] = Just First
      | s `elem` ["more", "up", "u"] = Just More
      | s `elem` ["less", "down", "d"] = Just Less
      | s `elem` ["last", "l"] = Just Last
      | otherwise = Nothing

startupTime :: DateTime
startupTime = unsafePerformIO $ getCurrentDateTime
{-# NOINLINE startupTime #-}

timeReader :: ReadM (Maybe LocalTime)
timeReader = (Just <$> toUtc <$> eitherReader readDateTime) <|> (maybeReader nothing)
  where
    toUtc :: DateTime -> LocalTime
    toUtc dt = LocalTime day time
      where
        day = dateTimeToDay dt
        time = TimeOfDay (hour dt) (minute dt) (fromIntegral $ second dt)

    nothing :: String -> Maybe (Maybe LocalTime)
    nothing "any" = Just Nothing
    nothing "now" = Just Nothing
    nothing "anytime" = Just Nothing
    nothing _ = Nothing

    readDateTime :: String -> Either String DateTime
    readDateTime str = case Parsec.runParser (do {t <- pDateTime startupTime; Parsec.eof; return t}) () "<start time>" str of
                         Left err -> Left (show err)
                         Right time -> Right time

queue :: Parser Command
queue = Queue
  <$> crudMode [(Update, "modify queue"), (Add, "create new queue"), (Delete, "delete queue")]
  <*> strArgument (metavar "QUEUE" <> help "queue to operate on")
  <*> optionalString "schedule" 's' "SCHEDULE" "queue schedule name"
  <*> optionalString "host" 'h' "HOST" "default host name for queue"
  <*> optionalString "name" 'n' "TITLE" "set queue title"
  <*> (   flag Nothing (Just True) (long "enable"  <> short 'e' <> help "enable queue")
      <|> flag Nothing (Just False) (long "disable" <> short 'D' <> help "disable queue")
      )
  <*> switch (long "force" <> short 'f' <> help "force non-empty queue deletion")

job :: Parser Command
job = Job
  <$> argument auto (metavar "ID" <> help "job ID")
  <*> optionalString "move" 'm' "QUEUE" "move job to other queue"
  <*> (optional $ option priorityReader (long "priority" <> short 'p' <> metavar "ACTION" <> help "change job priority. ACTION is one of: up, down, first, last"))
  <*> (optional $ option timeReader (long "at" <> long "start" <> metavar "YYYY-MM-DD HH:MM:SS" <> help "set job start time. Use `any' to specify that job can be run at any time."))
  <*> optionalString "status" 's' "STATUS" "set job status"
  <*> optionalString "host" 'h' "HOST" "set job host"
  <*> switch (long "description" <> help "view job description")
  <*> switch (long "result" <> short 'r' <> help "view job result")
  <*> switch (long "all" <> short 'a' <> help "view all job results")
  <*> crudMode [(View, "view job description or result"), (Update, "modify job"), (Delete, "delete job")]
    
schedule :: Parser Command
schedule = Schedule
  <$> crudMode [(View, "list available schedules"), (Add, "create new schedule"), (Update, "modify schedule"), (Delete, "delete (unused) schedule")]
  <*> many (strArgument (metavar "SCHEDULE" <> help "schedules to operate on"))
  <*> many (requiredString "period" 'p' "HH:MM:SS HH:MM:SS" "time of day period(s)")
  <*> many (required "weekday" 'w' "WEEKDAY" "week day(s)")
  <*> switch (long "force" <> short 'f' <> help "delete also all queues which use this schedule and their jobs")

user :: Parser Command
user = User
  <$> crudMode [(View, "list existing users"), (Add, "create new user"), (Update, "change user password")]
  <*> many (strArgument (metavar "NAME" <> help "name of user to operate on"))

grant :: Parser Command
grant = Grant
  <$> crudMode [(View, "view permissions of user"), (Add, "add permissions to user"), (Delete, "revoke permission from user")]
  <*> strArgument (metavar "NAME" <> help "name of user to operate on")
  <*> optionalF "id" 'i' "ID" "permission ID to operate on. mandatory for revoke operation."
  <*> optionalF "permission" 'p' "PERMISSION" "specify permission, for example ViewJobs"
  <*> optionalString "queue" 'q' "QUEUE" "queue to grant permission to, by default - any"
  <*> optionalString "type" 't' "TYPE" "job type to grant permission to, usable for CreateJobs permission"
  <*> optionalString "host" 'h' "HOST" "name of host to grant permission to, usable for CreateJobs permission. Use `__default__' value to allow default host of queue."

typesList :: Parser Command
typesList = Type
  <$> many (strArgument (metavar "TYPE"))

stats :: Parser Command
stats = Stats
  <$> many (strArgument (metavar "QUEUE"))

shell :: Parser Command
shell = pure Shell

parseParams :: [ParamDesc] -> Command -> JobParamInfo
parseParams desc e = 
    let posNames = map piName desc
        ordered = M.fromList $ zip posNames (jobCommand e)
        byName = M.fromList $ map parseOne (parameters e)
    in  M.union byName ordered
  where
    parseOne :: String -> (String, String)
    parseOne s = case break (== '=') s of
                   (key, (_:value)) -> (key, value)
                   (key, []) -> (key, "")

commands :: Parser Command
commands = hsubparser 
    (  cmd "enqueue" enqueue "put a new job into queue"
    <> cmd "ls" list "list queues or jobs"
    <> cmd "job" job "update or delete jobs"
    <> cmd "queue" queue "create, update or delete queues"
    <> cmd "schedule" schedule "create, update or delete schedules"
    <> cmd "type" typesList "view job types"
    <> cmd "stats" stats "print statistics on queue or on all jobs"
    <> cmd "user" user "create, update or delete users"
    <> cmd "grant" grant "create, update or delete user permissions"
    <> cmd "shell" shell "run interactive shell" )
  where
    cmd name func helpText = command name (info func (progDesc helpText))

parserInfo :: ParserInfo CmdLine
parserInfo = info (parser <**> helper)
               (fullDesc
               <> header "batch - the batchd toolset command-line client program"
               <> progDesc "operate on batch jobs, queues, schedules etc.")

getCmdArgs :: IO CmdLine
getCmdArgs = execParser parserInfo
