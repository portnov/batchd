{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings, FlexibleInstances, RecordWildCards #-}
-- | This module contains data type declarations that are used both by batchd daemon and client.
module Batchd.Core.Common.Types
  (
    -- * Data types
    Host (..), Variables,
    -- * Exceptions
    Error (..),
    -- * Utility functions
    bstrToString, stringToBstr,
    -- * Logging levels
    event_level, verbose_level, config_level
  ) where

import GHC.Generics
import Control.Exception
import Data.Generics hiding (Generic)
import Data.Char
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Yaml (ParseException (..))
import qualified System.Posix.Syslog as Syslog
import System.Log.Heavy

-- | Recognized error types.
data Error =
    QueueExists String
  | QueueNotExists String
  | QueueNotEmpty
  | ScheduleUsed
  | ScheduleNotExists String
  | JobNotExists
  | InvalidJobType ParseException
  | InvalidHost ParseException
  | InvalidDbCfg ParseException
  | InvalidHostControllerConfig ParseException
  | InvalidConfig ParseException
  | InvalidJobStatus (Maybe B.ByteString)
  | FileNotExists FilePath
  | MetricNotExists String
  | InsufficientRights String
  | InvalidStartTime
  | SqlError SomeException
  | HostInitializationError Int
  | UnknownError String

instance Show Error where
  show (QueueNotExists name) = "Queue does not exist: " ++ name
  show (QueueExists name) = "Queue already exists: " ++ name
  show QueueNotEmpty = "Queue is not empty"
  show (ScheduleNotExists name) = "Schedule does not exist: " ++ name
  show ScheduleUsed = "Schedule is used by queues"
  show JobNotExists = "Job does not exist"
  show (InvalidJobType e) = "Invalid job type: " ++ show e
  show (InvalidHost e) = "Invalid host description: " ++ show e
  show (InvalidDbCfg e) = "Invalid database config: " ++ show e
  show (InvalidHostControllerConfig e) = "Invalid host controller config: " ++ show e
  show (InvalidConfig e) = "Invalid config: " ++ show e
  show (InvalidJobStatus Nothing) = "Invalid job status"
  show (InvalidJobStatus (Just s)) = "Invalid job status: " ++ show s
  show (FileNotExists path) = "File does not exist: " ++ path
  show (MetricNotExists path) = "Metric does not exist: " ++ path
  show (InsufficientRights msg) = "Insufficient privileges: " ++ msg
  show InvalidStartTime = "Job start time does not match queue schedule"
  show (SqlError exc) = "SQL exception: " ++ show exc
  show (HostInitializationError rc) = "Failed to execute initialization commands on host: " ++ show rc
  show (UnknownError e) = "Unhandled error: " ++ e

instance Exception Error

type Variables = M.Map TL.Text T.Text

-- | Remote host description
data Host = Host {
    hName :: T.Text               -- ^ Name (identifier)
  , hHostName :: T.Text           -- ^ Network host name
  , hControllerId :: T.Text       -- ^ ID by which this host is known to the controller
  , hPublicKey :: Maybe FilePath    -- ^ Path to SSH public key file
  , hPrivateKey :: Maybe FilePath   -- ^ Path to SSH private key file
  , hPassphrase :: T.Text         -- ^ Passphrase for SSH private key
  , hRootPassword :: Maybe T.Text -- ^ Root user password - used only by some host drivers
  , hUserName :: T.Text           -- ^ SSH user name
  , hPort :: Int                  -- ^ SSH port (default 22)
  , hMaxJobs :: Maybe Int         -- ^ Maximum number of jobs which this host can execute
                                  --   in parallel.
  , hController :: T.Text         -- ^ Name of host controller. Default is local.
  , hStartupTime :: Int           -- ^ Startup\/initialization time, in seconds.
                                  --   Batchd will wait this time after host starttup
                                  --   before executing actual commands. Default is 5.
  , hShutdownTimeout :: Int       -- ^ Only shut down the host if it is not used for this
                                  --   time (in seconds). This is used to prevent too
                                  --   frequent shutdown\/start of one host. Default is 5*60.
  , hInputDirectory :: FilePath     -- ^ Directory (on the host) for input files. Default is @"."@.
  , hOutputDirectory :: FilePath    -- ^ Directory (on the host) with output files. Default is @"."@.
  , hScriptsDirectory :: Maybe FilePath -- ^ Directory (on the host) for temporary scripts.
  , hStartupHostCommands :: [T.Text]
  , hStartupDispatcherCommands :: [T.Text]
  , hVariables :: Variables
  }
  deriving (Eq, Show, Data, Typeable, Generic)

instance FromJSON Host where
  parseJSON (Object v) = do
    name <- v .: "name"
    host_name <- v .: "host_name"
    controller_id <- v .:? "controller_id" .!= name
    public_key <- v .:? "public_key"
    private_key <- v .:? "private_key"
    passphrase <- v .:? "passphrase" .!= ""
    root_password <- v .:? "root_password"
    user_name <- v .: "user_name"
    port <- v .:? "port" .!= 22
    max_jobs <- v .:? "max_jobs"
    controller <- v .:? "controller" .!= "local"
    startup_time <- v .:? "startup_time" .!= 5
    shutdown_timeout <- v .:? "shutdown_timeout" .!= (5*60)
    input_directory <- v .:? "input_directory" .!= "."
    output_directory <- v .:? "output_directory" .!= "."
    scripts_directory <- v .:? "scripts_directory"
    startup_host_commands <- v .:? "startup_commands_on_host" .!= []
    startup_dispatcher_commands <- v .:? "startup_commands_on_dispatcher" .!= []
    variables <- v .:? "variables" .!= M.empty
    return $ Host {
              hName = name
            , hHostName = host_name
            , hControllerId = controller_id
            , hPublicKey = public_key
            , hPrivateKey = private_key
            , hPassphrase = passphrase
            , hRootPassword = root_password
            , hUserName = user_name
            , hPort = port
            , hMaxJobs = max_jobs
            , hController = controller
            , hStartupTime = startup_time
            , hShutdownTimeout = shutdown_timeout
            , hInputDirectory = input_directory
            , hOutputDirectory = output_directory
            , hScriptsDirectory = scripts_directory
            , hStartupHostCommands = startup_host_commands
            , hStartupDispatcherCommands = startup_dispatcher_commands
            , hVariables = variables
            }
  parseJSON invalid = typeMismatch "host definition" invalid

-- | EVENT logging level
event_level :: Level
event_level = Level "EVENT" 350 Syslog.Info

-- | VERBOSE logging level
verbose_level :: Level
verbose_level = Level "VERBOSE" 450 Syslog.Info

-- | CONFIG logging level
config_level :: Level
config_level = Level "CONFIG" 700 Syslog.Debug

instance FromJSON Level where
  parseJSON (Aeson.String "config") = return config_level
  parseJSON (Aeson.String "debug") = return debug_level
  parseJSON (Aeson.String "verbose") = return verbose_level
  parseJSON (Aeson.String "info") = return info_level
  parseJSON (Aeson.String "warning") = return warn_level
  parseJSON (Aeson.String "error") = return error_level
  parseJSON (Aeson.String "fatal") = return fatal_level
  parseJSON (Aeson.String "disable") = return disable_logging
  parseJSON invalid = typeMismatch "logging level" invalid

instance ToJSON Level where
  toJSON l =
    case levelName l of
      "CONFIG" -> Aeson.String "config"
      "DEBUG" -> Aeson.String "debug"
      "VERBOSE" -> Aeson.String "verbose"
      "INFO" -> Aeson.String "info"
      "WARN" -> Aeson.String "warning"
      "ERROR" -> Aeson.String "error"
      "FATAL" -> Aeson.String "fatal"
      name -> Aeson.String name

-- | Utility conversion function. This assumes 1-byte encoding.
bstrToString :: B.ByteString -> String
bstrToString bstr = map (chr . fromIntegral) $ B.unpack bstr

-- | Utility conversion function. This assumes 1-byte encoding.
stringToBstr :: String -> B.ByteString
stringToBstr str = B.pack $ map (fromIntegral . ord) str

