{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings #-}

module Daemon.Types where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Logger.Syslog (runSyslogLoggingT)
import Control.Monad.Trans.Resource
import qualified Data.Text.Lazy as TL
import Database.Persist.Sql as Sql
import Web.Scotty.Trans as Scotty

import Common.CommonTypes

instance ScottyError Error where
  stringError e = UnknownError e
  showError e = TL.pack (show e)

type Result a = ExceptT Error IO a

type DB a = ReaderT SqlBackend (ExceptT Error (LoggingT (ResourceT IO))) a
type DBIO a = ReaderT SqlBackend (LoggingT (ResourceT IO)) a

throwR :: Error -> DB a
throwR ex = lift $ throwError ex

dbio :: DB a -> DBIO (Either Error a)
dbio action = do
  backend <- ask
  x <- lift $ runExceptT $ runReaderT action backend
  case x of
    Left err -> do
        return $ Left err
    Right r -> return $ Right r

data ConnectionInfo = ConnectionInfo {
    ciGlobalConfig :: GlobalConfig,
    ciPool :: Sql.ConnectionPool
  }

newtype ConnectionM a = ConnectionM {
    runConnection :: ReaderT ConnectionInfo IO a
  }
  deriving (Applicative,Functor,Monad,MonadIO, MonadReader ConnectionInfo)

type Action a = ActionT Error ConnectionM a

runDBA :: DB a -> Action a
runDBA qry = do
  pool <- lift (asks ciPool)
  cfg <- lift (asks ciGlobalConfig)
  r <- liftIO $ runResourceT $ (enableLogging cfg) (Sql.runSqlPool (dbio qry) pool)
  case r of
    Left err -> Scotty.raise err
    Right x -> return x

runDBA' :: DB a -> Action (Either Error a)
runDBA' qry = do
  pool <- lift (asks ciPool)
  cfg <- lift (asks ciGlobalConfig)
  r <- liftIO $ runResourceT $ (enableLogging cfg) (Sql.runSqlPool (dbio qry) pool)
  return r

runDB :: DB a -> ConnectionM (Either Error a)
runDB qry = do
  pool <- asks ciPool
  cfg <- asks ciGlobalConfig
  liftIO $ runResourceT $ (enableLogging cfg) $ Sql.runSqlPool (dbio qry) pool

runDBIO :: GlobalConfig -> ConnectionPool -> DB a -> IO (Either Error a)
runDBIO cfg pool qry = do
  runResourceT $ (enableLogging cfg) $ Sql.runSqlPool (dbio qry) pool

enableLogging cfg actions = runSyslogLoggingT $ filterLogger check actions
  where
    check _ level = level >= dbcLogLevel cfg

