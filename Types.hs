{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell #-}

module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import qualified Data.Map as M
import Data.Generics
import Data.Dates
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql

data ParamType =
    StringParam
  | IntParam
  | FileParam
  deriving (Eq, Show, Data, Typeable)

data Error =
    QueueExists
  | QueueNotExists
  | QueueNotEmpty
  | JobNotExists
  deriving (Eq, Show, Data, Typeable)

type Result a = ExceptT Error IO a

derivePersistField "WeekDay"

type DB a = ReaderT SqlBackend (ExceptT Error (NoLoggingT (ResourceT IO))) a
type DBIO a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

throwR :: Error -> DB a
throwR ex = lift $ throwError ex

dbio :: DB () -> DBIO ()
dbio action = do
  backend <- ask
  x <- lift $ runExceptT $ runReaderT action backend
  case x of
    Left err -> liftIO $ print err
    Right r -> return r
