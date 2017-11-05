{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings, FlexibleInstances, RecordWildCards #-}
-- | This module contains data type declarations that are used both by batchd daemon and client.
module Batchd.Core.Common.Types
  (
    -- * Data types
    Host (..),
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
import qualified Data.ByteString as B
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
  | InsufficientRights String
  | InvalidStartTime
  | SqlError SomeException
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
  show (InsufficientRights msg) = "Insufficient privileges: " ++ msg
  show InvalidStartTime = "Job start time does not match queue schedule"
  show (SqlError exc) = "SQL exception: " ++ show exc
  show (UnknownError e) = "Unhandled error: " ++ e

instance Exception Error

-- | Remote host description
data Host = Host {
    hName :: String               -- ^ Name (identifier)
  , hHostName :: String           -- ^ Network host name
  , hControllerId :: String       -- ^ ID by which this host is known to the controller
  , hPublicKey :: Maybe String    -- ^ Path to SSH public key file
  , hPrivateKey :: Maybe String   -- ^ Path to SSH private key file
  , hPassphrase :: String         -- ^ Passphrase for SSH private key
  , hUserName :: String           -- ^ SSH user name
  , hPort :: Int                  -- ^ SSH port (default 22)
  , hMaxJobs :: Maybe Int         -- ^ Maximum number of jobs which this host can execute
                                  --   in parallel.
  , hController :: String         -- ^ Name of host controller. Default is local.
  , hStartupTime :: Int           -- ^ Startup\/initialization time, in seconds.
                                  --   Batchd will wait this time after host starttup
                                  --   before executing actual commands. Default is 5.
  , hShutdownTimeout :: Int       -- ^ Only shut down the host if it is not used for this
                                  --   time (in seconds). This is used to prevent too
                                  --   frequent shutdown\/start of one host. Default is 5*60.
  , hInputDirectory :: String     -- ^ Directory (on the host) for input files. Default is @"."@.
  , hOutputDirectory :: String    -- ^ Directory (on the host) with output files. Default is @"."@.
  , hStartupCommands :: [String]
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
    user_name <- v .: "user_name"
    port <- v .:? "port" .!= 22
    max_jobs <- v .:? "max_jobs"
    controller <- v .:? "controller" .!= "local"
    startup_time <- v .:? "startup_time" .!= 5
    shutdown_timeout <- v .:? "shutdown_timeout" .!= (5*60)
    input_directory <- v .:? "input_directory" .!= "."
    output_directory <- v .:? "output_directory" .!= "."
    startup_commands <- v .:? "startup_commands" .!= []
    return $ Host {
              hName = name
            , hHostName = host_name
            , hControllerId = controller_id
            , hPublicKey = public_key
            , hPrivateKey = private_key
            , hPassphrase = passphrase
            , hUserName = user_name
            , hPort = port
            , hMaxJobs = max_jobs
            , hController = controller
            , hStartupTime = startup_time
            , hShutdownTimeout = shutdown_timeout
            , hInputDirectory = input_directory
            , hOutputDirectory = output_directory
            , hStartupCommands = startup_commands
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
config_level = Level "CONFIG" 700 Syslog.Info

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

