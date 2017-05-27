{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Daemon.Types where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
-- import Control.Monad.Logger.Syslog (runSyslogLoggingT)
import Control.Monad.Trans.Resource
import Control.Concurrent
import qualified Data.Text.Lazy as TL
import qualified Database.Persist.Sql as Sql
import qualified Web.Scotty.Trans as Scotty
import qualified System.Log.FastLogger as F
import System.Log.Heavy

import Common.Types

instance Scotty.ScottyError Error where
  stringError e = UnknownError e
  showError e = TL.pack (show e)

type Result a = ExceptT Error IO a

type DB a = ReaderT Sql.SqlBackend (LoggingT (ExceptT Error (ResourceT IO))) a
type DBIO a = ReaderT Sql.SqlBackend (LoggingT (ResourceT IO)) a

throwR :: Error -> DB a
throwR ex = lift $ lift $ throwError ex

dbio :: DB a -> DBIO (Either Error a)
dbio action = do
  backend <- ask
  logger <- lift ask
  x <- liftIO $ runResourceT $ runExceptT $ runLoggingTReader (runReaderT action backend) logger
  case x of
    Left err -> do
        return $ Left err
    Right r -> return $ Right r

data ConnectionInfo = ConnectionInfo {
    ciGlobalConfig :: GlobalConfig,
    ciPool :: Maybe Sql.ConnectionPool
  }

newtype Daemon a = Daemon {
    runConnectionM :: LoggingT (StateT ConnectionInfo IO) a
  }
  deriving (Applicative,Functor,Monad,MonadIO, MonadReader Logger)

runConnectionIO :: ConnectionInfo -> Logger -> Daemon a -> IO a
runConnectionIO connInfo logger actions =
  evalStateT (runLoggingTReader (runConnectionM actions) logger) connInfo

type Action a = Scotty.ActionT Error Daemon a

askConnectionInfo :: Daemon ConnectionInfo
askConnectionInfo = Daemon $ lift get

asksConnectionInfo :: (ConnectionInfo -> a) -> Action a
asksConnectionInfo fn = do
  ci <- lift askConnectionInfo
  return $ fn ci

askLoggerM :: Daemon Logger
askLoggerM = ask

askLogger :: Action Logger
askLogger = lift $ askLoggerM

askPool :: Daemon Sql.ConnectionPool
askPool = do
  mbPool <- Daemon $ lift $ gets ciPool
  case mbPool of
    Nothing -> fail $ "Database connection is not established yet"
    Just pool -> return pool

askConfig :: Daemon GlobalConfig
askConfig = Daemon $ lift $ gets ciGlobalConfig

askPoolA :: Action Sql.ConnectionPool
askPoolA = do
  mbPool <- asksConnectionInfo ciPool
  case mbPool of
    Nothing -> fail $ "Database connection is not established yet"
    Just pool -> return pool

askConfigA :: Action GlobalConfig
askConfigA = asksConnectionInfo ciGlobalConfig

runDBA :: DB a -> Action a
runDBA qry = do
  pool <- askPoolA
  cfg <- askConfigA
  logger <- askLogger
  r <- liftIO $ runResourceT $ runLoggingTReader (Sql.runSqlPool (dbio qry) pool) logger
  case r of
    Left err -> Scotty.raise err
    Right x -> return x

runDBA' :: DB a -> Action (Either Error a)
runDBA' qry = do
  pool <- askPoolA
  cfg <- asksConnectionInfo ciGlobalConfig
  logger <- askLogger
  r <- liftIO $ runResourceT $ runLoggingTReader (Sql.runSqlPool (dbio qry) pool) logger
  return r

runDB :: DB a -> Daemon (Either Error a)
runDB qry = do
  pool <- askPool
  cfg <- askConfig
  logger <- askLoggerM
  liftIO $ runResourceT $ runLoggingTReader (Sql.runSqlPool (dbio qry) pool) logger

runDBIO :: GlobalConfig -> Sql.ConnectionPool -> Logger -> DB a -> IO (Either Error a)
runDBIO cfg pool logger qry = do
  runResourceT $ runLoggingTReader (Sql.runSqlPool (dbio qry) pool) logger

runDaemon :: GlobalConfig -> Maybe Sql.ConnectionPool -> LogBackend -> Daemon a -> IO a
runDaemon cfg mbPool backend daemon =
    runner $ withLogging backend runner (runConnectionM daemon)
  where
    runner r = evalStateT r initState
    initState = ConnectionInfo cfg mbPool

wrapDaemon :: ((c -> IO a) -> IO a) -> (c -> Daemon a) -> Daemon a
wrapDaemon wrapper daemon = do
    cfg <- askConfig
    pool <- askPool
    logger <- askLoggerM
    let connInfo = ConnectionInfo cfg (Just pool)
    result <- liftIO $ wrapper $ \c -> runConnectionIO connInfo logger (daemon c)
    return result

forkDaemon :: Daemon a -> Daemon ()
forkDaemon daemon = do
    cfg <- askConfig
    pool <- askPool
    logger <- askLoggerM
    let connInfo = ConnectionInfo cfg (Just pool)
    liftIO $ forkIO $ do
               runConnectionIO connInfo logger daemon
               return ()
    return ()

