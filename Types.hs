{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Types where

import GHC.Generics
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Generics hiding (Generic)
import Data.Dates
import Data.Aeson
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql as Sql
import Web.Scotty.Trans as Scotty

data ParamType =
    StringParam
  | IntParam
  | FileParam
  deriving (Eq, Show, Data, Typeable)

data JobStatus =
    New
  | Processing
  | Done
  deriving (Eq, Show, Read, Data, Typeable, Generic)

instance ToJSON JobStatus
instance FromJSON JobStatus

data Error =
    QueueExists
  | QueueNotExists
  | QueueNotEmpty
  | JobNotExists
  | UnknownError String
  deriving (Eq, Show, Data, Typeable)

instance ScottyError Error where
  stringError e = UnknownError e
  showError e = T.pack (show e)

type Result a = ExceptT Error IO a

derivePersistField "WeekDay"
derivePersistField "JobStatus"

type DB a = ReaderT SqlBackend (ExceptT Error (NoLoggingT (ResourceT IO))) a
type DBIO a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

throwR :: Error -> DB a
throwR ex = lift $ throwError ex

dbio :: DB a -> DBIO (Either Error a)
dbio action = do
  backend <- ask
  x <- lift $ runExceptT $ runReaderT action backend
  case x of
    Left err -> do
        liftIO $ print err
        return $ Left err
    Right r -> return $ Right r

newtype ConnectionM a = ConnectionM {
    runConnection :: ReaderT Sql.ConnectionPool IO a
  }
  deriving (Applicative,Functor,Monad,MonadIO, MonadReader Sql.ConnectionPool)

type Action a = ActionT Error ConnectionM a

runDB :: DB a -> Action a
runDB qry = do
  pool <- lift ask
  r <- liftIO $ runResourceT $ runNoLoggingT (Sql.runSqlPool (dbio qry) pool)
  case r of
    Left err -> Scotty.raise err
    Right x -> return x

