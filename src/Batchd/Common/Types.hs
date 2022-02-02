{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
-- | This module contains data type declarations that are used both by batchd daemon and client.
module Batchd.Common.Types
  (
    -- * Data types
    JobType (..), OnFailAction (..),
    ParamType (..), ParamDesc (..),
    JobStatus (..),
    JobParamInfo, JobInfo (..),
    UserInfo (..), Permission (..),
    DbDriver (..), DaemonMode (..),
    AuthMode (..), AuthMethod (..),
    LogTarget (..), LogConfig (..),
    WebClientConfig (..),
    ManagerConfig (..), DispatcherConfig (..),
    MetricsConfig (..),
    StorageConfig (..),
    GlobalConfig (..),
    MetricKind (..),
    ByStatus (..),
    -- * Exceptions
    UploadException (..), DownloadException (..), ExecException (..),
    -- * Utility functions
    lookupParam, getParamType,
    parseUpdate, parseUpdateMaybe,
    parseUpdateStar, parseStatus,
    jsonOptions, authMethods,
    -- * Some default settings
    defaultManagerPort, zeroUtcTime,
    defaultAuthMode, defaultLogConfig,
    defaultLogFormat, defaultStaticSalt,
    getAuthStaticSalt
  ) where

import GHC.Generics
import Control.Exception
import Control.Monad
import Data.Generics hiding (Generic)
import Data.Int
import Data.Char
import Data.String
import Data.Time
import Data.List (isPrefixOf)
import Database.Persist
import Database.Persist.TH
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Default
import qualified Data.Text.Format.Heavy as F
import qualified Data.Text.Format.Heavy.Parse.Braces as PF
import Data.Dates
import qualified System.Posix.Syslog as Syslog
import System.Log.Heavy
import System.Exit

import Batchd.Core.Common.Types

-- | Default manager port - 9681.
defaultManagerPort :: Int
defaultManagerPort = 9681

-- | Type of job parameter
data ParamType =
    String
  | Integer
  | InputFile
  | OutputFile
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Description of job parameter
data ParamDesc = ParamDesc {
    piName :: TL.Text       -- ^ Parameter name (identifier)
  , piType :: ParamType    -- ^ Parameter type
  , piTitle :: TL.Text      -- ^ Parameter title (to show in client)
  , piDefault :: T.Text    -- ^ Default value of the parameter
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Job type description
data JobType = JobType {
    jtName :: String            -- ^ Job type name (identifier)
  , jtTitle :: Maybe String     -- ^ Job type title (to show in client)
  , jtTemplate :: String        -- ^ Template of command line to execute
  , jtOnFail :: OnFailAction    -- ^ What to do if execution failed
  , jtHostName :: Maybe String  -- ^ Name of host where to execute jobs.
                                --   Nothing means use default host from queue or localhost.
  , jtMaxJobs :: Maybe Int      -- ^ Maximum count of jobs of this type which
                                --   can be executed in parallel on one host.
                                --   Can be overruled in host settings.
  , jtParams :: [ParamDesc]     -- ^ Set of job parameters
  }
  deriving (Eq, Show, Data, Typeable, Generic)

stripPrefix :: String -> String -> String
stripPrefix prefix str =
  if prefix `isPrefixOf` str
    then drop (length prefix) str
    else str

camelCaseToUnderscore :: String -> String
camelCaseToUnderscore = go False
  where
    go _ [] = []
    go False (x:xs) = toLower x : go True xs
    go True (x:xs)
      | isUpper x = '_' : toLower x : go True xs
      | otherwise = x : go True xs

jsonOptions :: String -> Data.Aeson.Types.Options
jsonOptions prefix = defaultOptions {fieldLabelModifier = camelCaseToUnderscore . stripPrefix prefix}

instance FromJSON ParamType where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToUnderscore}

instance ToJSON ParamType where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToUnderscore}

instance FromJSON ParamDesc where
  parseJSON (Object v) = do
    name <- v .: "name" 
    tp <- v .: "type"
    title <- v .:? "title" .!= name
    dflt <- v .:? "default" .!= ""
    return $ ParamDesc name tp title dflt
  parseJSON invalid = typeMismatch "parameter description" invalid

instance ToJSON ParamDesc where
  toJSON = genericToJSON (jsonOptions "pi")

instance FromJSON JobType where
  parseJSON = genericParseJSON (jsonOptions "jt")

instance ToJSON JobType where
  toJSON = genericToJSON (jsonOptions "jt")

-- | What to do if job execution fails
data OnFailAction =
    Continue       -- ^ Continue to the next job
  | RetryNow Int   -- ^ Leave the job in the queue and retry execution.
                   --   Not more than @n@ times.
  | RetryLater Int -- ^ Put the job to the end of queue, to be executed later.
                   --   Not more than @n@ times.
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance ToJSON OnFailAction where
  toJSON Continue = Aeson.String "continue"
  toJSON (RetryNow n) = object ["retry" .= object ["when" .= ("now" :: T.Text), "count" .= n]]
  toJSON (RetryLater n) = object ["retry" .= object ["when" .= ("later" :: T.Text), "count" .= n]]

instance FromJSON OnFailAction where
  parseJSON (Aeson.String "continue") = return Continue
  parseJSON (Aeson.String "retry") = return (RetryNow 1)
  parseJSON (Object v) = do
    r <- v .: "retry"
    case r of
      Object retry -> do
          nowStr <- retry .:? "when" .!= "now"
          now <- case nowStr of
                   "now" -> return True
                   "later" -> return False
                   _ -> fail $ "Unknown retry type specification: " ++ nowStr
          count <- retry .:? "count" .!= 1
          if now
            then return $ RetryNow count
            else return $ RetryLater count
      _ -> typeMismatch "retry" r
  parseJSON invalid = typeMismatch "on fail" invalid

-- | Job execution status
data JobStatus =
    New         -- ^ Just created, waiting for polling process to peek it.
  | Waiting     -- ^ Waiting for free worker.
  | Processing  -- ^ Being processed by the worker.
  | Done        -- ^ Successfully executed.
  | Failed      -- ^ Execution failed.
  | Postponed   -- ^ Execution postponed. This status can be set only manually.
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance ToJSON JobStatus
instance FromJSON JobStatus

newtype ByStatus a = ByStatus (M.Map JobStatus a)
  deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON a => ToJSON (ByStatus a) where
  toJSON (ByStatus m) = object $ map go $ M.assocs m
    where
      go (st,x) = (T.pack $ map toLower $ show st) .= toJSON x

instance FromJSON a => FromJSON (ByStatus a) where
  parseJSON (Object v) = do
      pairs <- forM (H.toList v) go
      return $ ByStatus $ M.fromList pairs
    where
      go (key,val) = do
        Just st <- parseStatus Nothing (fail "invalid status") (Just key) 
        cnt <- parseJSON val
        return (st, cnt)

deriving instance Generic WeekDay
instance ToJSON WeekDay
instance FromJSON WeekDay

-- | Job parameter values
type JobParamInfo = Variables

-- | Job information. This data type
-- unites information from @Job@ and connected
-- @JobParam@.
data JobInfo = JobInfo {
    jiId :: Int64,
    jiQueue :: String,
    jiType :: String,
    jiSeq :: Int,
    jiUserName :: String,
    jiCreateTime :: UTCTime,
    jiStartTime :: Maybe UTCTime,
    jiStatus :: JobStatus,
    jiTryCount :: Int,
    jiHostName :: Maybe String,
    jiNotes :: Maybe String,
    jiResultTime :: Maybe UTCTime,
    jiExitCode :: Maybe ExitCode,
    jiStdout :: Maybe T.Text,
    jiStderr :: Maybe T.Text,
    jiParams :: JobParamInfo
  }
  deriving (Generic, Show)

instance ToJSON JobInfo where
  toJSON = genericToJSON (jsonOptions "ji")

zeroUtcTime :: UTCTime
zeroUtcTime = UTCTime (ModifiedJulianDay 0) 0

instance FromJSON JobInfo where
  parseJSON (Object v) =
    JobInfo
      <$> v .:? "id" .!= 0
      <*> v .:? "queue" .!= ""
      <*> v .: "type"
      <*> v .:? "seq" .!= 0
      <*> v .:? "user_name" .!= "<unknown>"
      <*> v .:? "create_time" .!= zeroUtcTime
      <*> v .:? "start_time"
      <*> v .:? "status" .!= New
      <*> v .:? "try_count" .!= 0
      <*> v .:? "host_name"
      <*> v .:? "notes"
      <*> v .:? "result_time"
      <*> v .:? "exit_code"
      <*> v .:? "stdout"
      <*> v .:? "stderr"
      <*> v .:? "params" .!= M.empty
  parseJSON invalid = typeMismatch "job" invalid

-- | User name and password
data UserInfo = UserInfo {
    uiName :: String,
    uiPassword :: String
  } deriving (Generic, Show)

instance ToJSON UserInfo where
  toJSON = genericToJSON (jsonOptions "ui")

instance FromJSON UserInfo where
  parseJSON = genericParseJSON (jsonOptions "ui")

-- | Lookup for parameter description by name.
lookupParam :: TL.Text       -- ^ Parameter name
            -> [ParamDesc]  -- ^ List of parameter descriptions
            -> Maybe ParamDesc
lookupParam _ [] = Nothing
lookupParam name (p:ps)
  | piName p == name = Just p
  | otherwise = lookupParam name ps

-- | Lookup for parameter type by name
getParamType :: JobType -> TL.Text -> Maybe ParamType
getParamType jt name = piType `fmap` lookupParam name (jtParams jt)

-- | Supported database drivers
data DbDriver = Sqlite | PostgreSql
  deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON DbDriver
instance FromJSON DbDriver

-- | Daemon execution mode
data DaemonMode =
    Manager     -- ^ Manager thread only
  | Dispatcher  -- ^ Dispatcher thread only
  | Both        -- ^ Both manager and dispatcher threads
  deriving (Data, Typeable, Show, Read, Eq, Ord, Generic)

instance ToJSON DaemonMode
instance FromJSON DaemonMode

-- | Authentication methods
data AuthMode =
    AuthDisabled              -- ^ Authentication is disabled, all users to be authorized as superusers
  | AuthConfig {
    authBasicEnabled :: Bool  -- ^ Is HTTP Basic authentication enabled
  , authHeaderEnabled :: Bool -- ^ Is use of @X-Auth-User:@ HTTP header enabled
  , authStaticSalt :: String  -- ^ Static salt value
  }
  deriving (Data, Typeable, Show, Eq, Generic)

getAuthStaticSalt :: AuthMode -> String
getAuthStaticSalt AuthDisabled = defaultStaticSalt
getAuthStaticSalt auth = authStaticSalt auth

instance ToJSON AuthMode where
  toJSON AuthDisabled = Aeson.String "disable"
  toJSON (AuthConfig {..}) = object ["basic" .= authBasicEnabled, "header" .= authHeaderEnabled, "staticSalt" .= authStaticSalt]

instance FromJSON AuthMode where
  parseJSON (Aeson.String "disable") = return AuthDisabled
  parseJSON (Object v) =
    AuthConfig
      <$> v .:? "basic" .!= True
      <*> v .:? "header" .!= False
      <*> v .:? "static_salt" .!= defaultStaticSalt

-- | Default autentication mode: only HTTP basic auth is enabled.
defaultAuthMode :: AuthMode
defaultAuthMode = AuthConfig True False defaultStaticSalt

-- | Supported authentication methods
data AuthMethod = BasicAuth | HeaderAuth
  deriving (Data, Typeable, Show, Read, Eq, Generic)

instance ToJSON AuthMethod where
  toJSON BasicAuth = Aeson.String "basic"
  toJSON HeaderAuth = Aeson.String "header"

instance FromJSON AuthMethod where
  parseJSON (Aeson.String "basic") = return BasicAuth
  parseJSON (Aeson.String "header") = return HeaderAuth
  parseJSON x = typeMismatch "auth method" x

-- | Get enabled authentication methods from specified mode.
authMethods :: AuthMode -> [AuthMethod]
authMethods AuthDisabled = []
authMethods (AuthConfig {..}) =
  (if authBasicEnabled then [BasicAuth] else []) ++
  (if authHeaderEnabled then [HeaderAuth] else [])

-- | Supported logging targets
data LogTarget =
    LogSyslog
  | LogStdout
  | LogStderr
  | LogFile FilePath
  deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON LogTarget where
  toJSON LogSyslog = Aeson.String "syslog"
  toJSON LogStdout = Aeson.String "stdout"
  toJSON LogStderr = Aeson.String "stderr"
  toJSON (LogFile path) = toJSON path

instance FromJSON LogTarget where
  parseJSON (Aeson.String "syslog") = return LogSyslog
  parseJSON (Aeson.String "stdout") = return LogStdout
  parseJSON (Aeson.String "stderr") = return LogStderr
  parseJSON (Aeson.String path) = return $ LogFile $ T.unpack path
  parseJSON invalid = typeMismatch "log target" invalid

-- | Logging configuration
data LogConfig = LogConfig {
    lcTarget :: LogTarget            -- ^ Logging target. Default is syslog.
  , lcFormat :: F.Format             -- ^ Log message format.
  , lcLevel :: Level                 -- ^ General filter
  , lcFilter :: [(String, Level)]    -- ^ Filter by source prefixes
  }
  deriving (Eq, Show, Typeable, Generic)

-- | Default logging config
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig LogSyslog defaultLogFormat info_level []

-- | Default log messages format:
-- @"{time} [{level:~l}] {source} ({fullcontext}): {message}\n"@
defaultLogFormat :: F.Format
defaultLogFormat = "{time} [{level:~l}] {source} ({fullcontext}): {message}\n"

instance ToJSON LogConfig where
  toJSON = genericToJSON (jsonOptions "lc")

instance ToJSON F.Format where
  toJSON fmt = toJSON (show fmt)

instance FromJSON LogConfig where
  parseJSON (Object v) = LogConfig
    <$> v .:? "target" .!= LogSyslog
    <*> parseLogFormat (v .:? "format")
    <*> v .:? "level" .!= info_level
    <*> parseFilter (v .:? "filter" .!= M.empty)
    where
      parseFilter :: Parser (M.Map String Level) -> Parser [(String, Level)]
      parseFilter = fmap M.assocs

      parseLogFormat :: Parser (Maybe TL.Text) -> Parser F.Format
      parseLogFormat p = do
        mbString <- p
        case mbString of
          Nothing -> return defaultLogFormat
          Just str -> case PF.parseFormat str of
                        Left err -> fail $ show err
                        Right fmt -> return fmt

-- | Global daemon configuration
data GlobalConfig = GlobalConfig {
      dbcDaemonMode :: DaemonMode      -- ^ Daemon execution mode
    , dbcDriver :: DbDriver            -- ^ Type of DB backend
    , dbcConnectionString :: T.Text    -- ^ DB connection string
    , dbcLogging :: LogConfig          -- ^ Logging configuration
    , dbcManager :: ManagerConfig
    , dbcDispatcher :: DispatcherConfig
    , dbcMetrics :: MetricsConfig
    , dbcStorage :: StorageConfig
    , dbcVariables :: Variables
  }
  deriving (Eq, Show, Typeable, Generic)

instance Default GlobalConfig where
  def = GlobalConfig {
          dbcDaemonMode = Both
        , dbcDriver = Sqlite
        , dbcConnectionString = ":memory:"
        , dbcLogging = defaultLogConfig
        , dbcManager = def
        , dbcDispatcher = def
        , dbcMetrics = def
        , dbcStorage = def
        , dbcVariables = def
        }

data MetricsConfig = MetricsConfig {
    mcGcMetrics :: Bool     -- ^ Whether to enable GC metrics (default true)
  , mcHttpMetrics :: Bool   -- ^ Whether to enable HTTP metrics for manager REST API (default true)
  , mcStorePrefixOnly :: Maybe T.Text -- ^ Only store metrics, names of which starts with specified prefix.
  , mcDumpTimeout :: Int    -- ^ How often to dump metrics data to DB, in seconds (default - each 10 seconds)
  }
  deriving (Eq, Show, Typeable, Generic)

instance Default MetricsConfig where
  def = MetricsConfig {
          mcGcMetrics = True
        , mcHttpMetrics = True
        , mcStorePrefixOnly = Nothing
        , mcDumpTimeout = 10
        }

data WebClientConfig = WebClientConfig {
    wcAllowedOrigin :: Maybe String  -- ^ Allowed Origin for CORS
  , wcPath :: String                 -- ^ Path to web client HTML\/JS\/CSS files
  }
  deriving (Eq, Show, Typeable, Generic)

instance Default WebClientConfig where
  def = WebClientConfig Nothing ""

data ManagerConfig = ManagerConfig {
    mcPort :: Int                  -- ^ Network port for manager to listen
  , mcAuth :: AuthMode             -- ^ Authentication configuration
  , mcWebClient :: Maybe WebClientConfig
  }
  deriving (Eq, Show, Typeable, Generic)

instance Default ManagerConfig where
  def = ManagerConfig {
          mcPort = defaultManagerPort
        , mcAuth = defaultAuthMode
        , mcWebClient = Nothing
        }

data DispatcherConfig = DispatcherConfig {
    dcWorkers :: Int
  , dcPollTimeout :: Int
  }
  deriving (Eq, Show, Typeable, Generic)

instance Default DispatcherConfig where
  def = DispatcherConfig {
          dcWorkers = 1
        , dcPollTimeout = 10
        }

data StorageConfig = StorageConfig {
    scDoneJobs :: Int      -- ^ How long to store executed jobs, in days.
  , scMetricRecords :: Int -- ^ How long to store metric records, in days.
  }
  deriving (Eq, Show, Typeable, Generic)

instance Default StorageConfig where
  def = StorageConfig {
          scDoneJobs = 2
        , scMetricRecords = 3
        }

-- | Default static salt value.
defaultStaticSalt :: String
defaultStaticSalt = "1234567890abcdef"

instance ToJSON MetricsConfig where
  toJSON = genericToJSON (jsonOptions "mc")

instance FromJSON MetricsConfig where
  parseJSON (Object v) =
    MetricsConfig
      <$> v .:? "gc_metrics" .!= mcGcMetrics def
      <*> v .:? "http_metrics" .!= mcHttpMetrics def
      <*> v .:? "store_prefix_only" .!= mcStorePrefixOnly def
      <*> v .:? "dump_timeout" .!= mcDumpTimeout def
  parseJSON invalid = typeMismatch "metrics configuration" invalid

instance ToJSON WebClientConfig where
  toJSON = genericToJSON (jsonOptions "wc")

instance FromJSON WebClientConfig where
  parseJSON (Object v) =
    WebClientConfig
      <$> v .:? "allowed_origin"
      <*> v .: "path"
  parseJSON invalid = typeMismatch "web client configuration" invalid

instance ToJSON ManagerConfig where
  toJSON = genericToJSON (jsonOptions "mc")

instance FromJSON ManagerConfig where
  parseJSON (Object v) =
    ManagerConfig
      <$> v .:? "port" .!= defaultManagerPort
      <*> v .:? "auth" .!= defaultAuthMode
      <*> v .:? "web_client"
  parseJSON invalid = typeMismatch "manager configuration" invalid

instance ToJSON DispatcherConfig where
  toJSON = genericToJSON (jsonOptions "dc")
  
instance FromJSON DispatcherConfig where
  parseJSON (Object v) =
    DispatcherConfig
      <$> v .:? "workers" .!= dcWorkers def
      <*> v .:? "poll_timeout" .!= dcPollTimeout def
  parseJSON invalid = typeMismatch "dispatcher configuration" invalid

instance ToJSON StorageConfig where
  toJSON = genericToJSON (jsonOptions "sc")
  
instance FromJSON StorageConfig where
  parseJSON (Object v) =
    StorageConfig
      <$> v .:? "done_jobs" .!= scDoneJobs def
      <*> v .:? "metric_records" .!= scMetricRecords def
  parseJSON invalid = typeMismatch "storage configuration" invalid

instance ToJSON GlobalConfig where
  toJSON = genericToJSON (jsonOptions "dbc")

instance FromJSON GlobalConfig where
  parseJSON (Object v) =
    GlobalConfig
      <$> v .:? "daemon" .!= dbcDaemonMode def
      <*> v .:? "driver" .!= dbcDriver def
      <*> v .:? "connection_string" .!= dbcConnectionString def
      <*> v .:? "logging" .!= defaultLogConfig
      <*> v .:? "manager" .!= def
      <*> v .:? "dispatcher" .!= def
      <*> v .:? "metrics" .!= def
      <*> v .:? "storage" .!= def
      <*> v .:? "variables" .!= def
  parseJSON invalid = typeMismatch "global configuration" invalid

data MetricKind =
    Counter
  | Gauge
  | Label
  | Distribution
  deriving (Eq, Ord, Show, Read)

deriving instance Data Syslog.Priority
deriving instance Typeable Syslog.Priority

deriving instance Data Level
deriving instance Typeable Level

parseStatus :: (Eq s, IsString s, Monad m) => Maybe JobStatus -> m (Maybe JobStatus) -> Maybe s -> m (Maybe JobStatus)
parseStatus dflt _ Nothing = return dflt
parseStatus _ _ (Just "all") = return Nothing
parseStatus _ _ (Just "new") = return $ Just New
parseStatus _ _ (Just "waiting") = return $ Just Waiting
parseStatus _ _ (Just "processing") = return $ Just Processing
parseStatus _ _ (Just "done") = return $ Just Done
parseStatus _ _ (Just "failed") = return $ Just Failed
parseStatus _ _ (Just "postponed") = return $ Just Postponed
parseStatus _ handle (Just _) = handle

instance ToJSON ExitCode where
  toJSON ExitSuccess = Number (fromIntegral 0)
  toJSON (ExitFailure n) = Number (fromIntegral n)

instance FromJSON ExitCode where
  parseJSON (Number 0) = return ExitSuccess
  parseJSON (Number n) = return $ ExitFailure $ round n
  parseJSON x = typeMismatch "exit code" x

data ExecException = ExecException SomeException
  deriving (Typeable)

instance Exception ExecException

instance Show ExecException where
  show (ExecException e) = "Exception during command execution: " ++ show e

data UploadException = UploadException FilePath SomeException
  deriving (Typeable)

instance Exception UploadException

instance Show UploadException where
  show (UploadException path e) = "Exception uploading file `" ++ path ++ "': " ++ show e

data DownloadException = DownloadException FilePath SomeException
  deriving (Typeable)

instance Exception DownloadException

instance Show DownloadException where
  show (DownloadException path e) = "Exception downloading file `" ++ path ++ "': " ++ show e

derivePersistField "WeekDay"
derivePersistField "JobStatus"
derivePersistField "ExitCode"
derivePersistField "DaemonMode"
derivePersistField "MetricKind"

-- | Supported user permissions.
data Permission =
    SuperUser
  | CreateJobs
  | ViewJobs
  | ManageJobs
  | ViewQueues
  | ManageQueues
  | ViewSchedules
  | ManageSchedules
  deriving (Eq, Show, Read, Data, Typeable, Generic)

derivePersistField "Permission"

instance FromJSON Permission where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToUnderscore}

instance ToJSON Permission where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToUnderscore}

parseUpdate :: (PersistField t, FromJSON t) => EntityField v t -> T.Text -> Value -> Parser (Maybe (Update v))
parseUpdate field label (Object v) = do
  mbValue <- v .:? label
  let upd = case mbValue of
              Nothing -> Nothing
              Just value -> Just (field =. value)
  return upd

parseUpdateMaybe :: (PersistField t, FromJSON t) => EntityField v (Maybe t) -> T.Text -> Value -> Parser (Maybe (Update v))
parseUpdateMaybe field label (Object v) = do
  if label `H.member` v
    then do
      mbValue <- v .:? label
      return $ Just $ field =. mbValue
    else return Nothing

parseUpdateStar :: (PersistField t, FromJSON t, IsString t, Eq t)
             => EntityField v (Maybe t) -> T.Text -> Value -> Parser (Maybe (Update v))
parseUpdateStar field label (Object v) = do
  mbValue <- v .:? label
  let upd = case mbValue of
              Nothing -> Nothing
              Just "*" -> Just (field =. Nothing)
              Just value -> Just (field =. Just value)
  return upd

