{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings, FlexibleInstances #-}

module CommonTypes where

import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.Logger
import Data.Generics hiding (Generic)
import Data.Int
import Data.Char
import Data.String
import Data.Time
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Yaml (ParseException (..))
import Data.Dates

defaultManagerPort :: Int
defaultManagerPort = 9681

data ParamType =
    String
  | Integer
  | InputFile
  | OutputFile
  deriving (Eq, Show, Data, Typeable, Generic)

data JobType = JobType {
    jtName :: String,
    jtTemplate :: String,
    jtOnFail :: OnFailAction,
    jtHostName :: Maybe String,
    jtParams :: M.Map String ParamType
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

instance FromJSON JobType where
  parseJSON = genericParseJSON (jsonOptions "jt")

instance FromJSON ParamType where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelCaseToUnderscore}

instance ToJSON JobType where
  toJSON = genericToJSON (jsonOptions "jt")

instance ToJSON ParamType where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = camelCaseToUnderscore}

data OnFailAction =
    Continue
  | RetryNow Int
  | RetryLater Int
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
          now <- retry .:? "when" .!= True
          count <- retry .:? "count" .!= 1
          if now
            then return $ RetryNow count
            else return $ RetryLater count
      _ -> typeMismatch "retry" r
  parseJSON invalid = typeMismatch "on fail" invalid

data JobStatus =
    New
  | Processing
  | Done
  | Failed
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance ToJSON JobStatus
instance FromJSON JobStatus

instance ToJSON a => ToJSON (M.Map JobStatus a) where
  toJSON m = object $ map go $ M.assocs m
    where
      go (st,x) = (T.pack $ map toLower $ show st) .= toJSON x

instance FromJSON a => FromJSON (M.Map JobStatus a) where
  parseJSON (Object v) = do
      pairs <- forM (H.toList v) go
      return $ M.fromList pairs
    where
      go (key,val) = do
        Just st <- parseStatus Nothing (fail "invalid status") (Just key) 
        cnt <- parseJSON val
        return (st, cnt)

deriving instance Generic WeekDay
instance ToJSON WeekDay
instance FromJSON WeekDay

data Error =
    QueueExists
  | QueueNotExists
  | QueueNotEmpty
  | ScheduleUsed
  | JobNotExists
  | InvalidJobType ParseException
  | InvalidHost ParseException
  | InvalidDbCfg ParseException
  | InvalidJobStatus
  | FileNotExists
  | UnknownError String
  deriving (Show)

type JobParamInfo = M.Map String String

data JobInfo = JobInfo {
    jiId :: Int64,
    jiQueue :: String,
    jiType :: String,
    jiSeq :: Int,
    jiTime :: UTCTime,
    jiStatus :: JobStatus,
    jiTryCount :: Int,
    jiHostName :: Maybe String,
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
      <*> v .:? "time" .!= zeroUtcTime
      <*> v .:? "status" .!= New
      <*> v .:? "try_count" .!= 0
      <*> v .:? "host_name"
      <*> v .:? "params" .!= M.empty
  parseJSON invalid = typeMismatch "job" invalid

getParamType :: JobType -> String -> Maybe ParamType
getParamType jt name = M.lookup name (jtParams jt)

-- | Remote host description
data Host = Host {
    hName :: String,
    hHostName :: String,
    hPublicKey :: Maybe String,
    hPrivateKey :: Maybe String,
    hPassphrase :: String,
    hUserName :: String,
    hPort :: Int,
    hInputDirectory :: String,
    hOutputDirectory :: String,
    hStartupCommands :: [String]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

instance FromJSON Host where
  parseJSON (Object v) =
    Host
      <$> v .: "name"
      <*> v .: "host_name"
      <*> v .:? "public_key"
      <*> v .:? "private_key"
      <*> v .:? "passphrase" .!= ""
      <*> v .: "user_name"
      <*> v .:? "port" .!= 22
      <*> v .:? "input_directory" .!= "."
      <*> v .:? "output_directory" .!= "."
      <*> v .:? "startup_commands" .!= []
  parseJSON invalid = typeMismatch "host definition" invalid

-- | Supported database drivers
data DbDriver = Sqlite | PostgreSql
  deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON DbDriver
instance FromJSON DbDriver

data DaemonMode =
    Manager
  | Dispatcher
  | Both
  deriving (Data, Typeable, Show, Eq, Generic)

instance ToJSON DaemonMode
instance FromJSON DaemonMode

data GlobalConfig = GlobalConfig {
    dbcDaemonMode :: DaemonMode,
    dbcManagerPort :: Int,
    dbcDriver :: DbDriver,
    dbcConnectionString :: T.Text,
    dbcWorkers :: Int,
    dbcPollTimeout :: Int,
    dbcLogLevel :: LogLevel,
    dbcStoreDone :: Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

instance ToJSON GlobalConfig where
  toJSON = genericToJSON (jsonOptions "dbc")

instance FromJSON GlobalConfig where
  parseJSON (Object v) =
    GlobalConfig
      <$> v .:? "daemon" .!= Both
      <*> v .:? "manager_port" .!= defaultManagerPort
      <*> v .:? "driver" .!= Sqlite
      <*> v .:? "connection_string" .!= ":memory"
      <*> v .:? "workers" .!= 1
      <*> v .:? "poll_timeout" .!= 10
      <*> v .:? "log_level" .!= LevelInfo
      <*> v .:? "store_done" .!= 2
  parseJSON invalid = typeMismatch "global configuration" invalid

instance FromJSON LogLevel where
  parseJSON (Aeson.String "debug") = return LevelDebug
  parseJSON (Aeson.String "info") = return LevelInfo
  parseJSON (Aeson.String "warning") = return LevelWarn
  parseJSON (Aeson.String "error") = return LevelError
  parseJSON invalid = typeMismatch "logging level" invalid

instance ToJSON LogLevel where
  toJSON LevelDebug = Aeson.String "debug"
  toJSON LevelInfo  = Aeson.String "info"
  toJSON LevelWarn  = Aeson.String "warning"
  toJSON LevelError = Aeson.String "error"
  toJSON (LevelOther x) = Aeson.String x
  

deriving instance Data LogLevel
deriving instance Typeable LogLevel

parseStatus :: (Eq s, IsString s, Monad m) => Maybe JobStatus -> m (Maybe JobStatus) -> Maybe s -> m (Maybe JobStatus)
parseStatus dflt _ Nothing = return dflt
parseStatus _ _ (Just "all") = return Nothing
parseStatus _ _ (Just "new") = return $ Just New
parseStatus _ _ (Just "processing") = return $ Just Processing
parseStatus _ _ (Just "done") = return $ Just Done
parseStatus _ _ (Just "failed") = return $ Just Failed
parseStatus _ handle (Just _) = handle

