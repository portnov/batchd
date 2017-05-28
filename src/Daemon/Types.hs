{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Daemon.Types where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Concurrent
import qualified Data.Text.Lazy as TL
import qualified Database.Persist.Sql as Sql
import qualified Web.Scotty.Trans as Scotty
import System.Log.Heavy

import Common.Types

instance Scotty.ScottyError Error where
  stringError e = UnknownError e
  showError e = TL.pack (show e)

-- | Standard monad for database actions
type DB a = ReaderT Sql.SqlBackend (LoggingT (ExceptT Error (ResourceT IO))) a

-- | Temporary monad type for usage of DB from IO directly
type DBIO a = ReaderT Sql.SqlBackend (LoggingT (ResourceT IO)) a

-- | Throw an error in DB monad
throwR :: Error -> DB a
throwR ex = lift $ lift $ throwError ex

-- | Convert DB monad into DBIO
dbio :: DB a -> DBIO (Either Error a)
dbio action = do
  backend <- ask
  logger <- lift ask
  x <- liftIO $ runResourceT $ runExceptT $ runLoggingT (runReaderT action backend) logger
  case x of
    Left err -> do
        return $ Left err
    Right r -> return $ Right r

-- | Database connection information and configuration
data ConnectionInfo = ConnectionInfo {
    ciGlobalConfig :: GlobalConfig     -- ^ Global configuration
  , ciPool :: Maybe Sql.ConnectionPool -- ^ DB connection pool
  }

-- | Main monad for daemon actions (both Manager and Dispatcher). This handles logging
-- and DB connection.
newtype Daemon a = Daemon {
    runDaemonT :: LoggingT (StateT ConnectionInfo IO) a
  }
  deriving (Applicative,Functor,Monad,MonadIO, MonadReader Logger)

-- | Run daemon actions within IO monad
runDaemonIO :: ConnectionInfo -> Logger -> Daemon a -> IO a
runDaemonIO connInfo logger actions =
  evalStateT (runLoggingT (runDaemonT actions) logger) connInfo

-- | REST handler monad type
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

-- | Run DB action within Action monad. Raise HTTP-level error if DB action fails.
runDBA :: DB a -> Action a
runDBA qry = do
  pool <- askPoolA
  cfg <- askConfigA
  logger <- askLogger
  r <- liftIO $ runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) logger
  case r of
    Left err -> Scotty.raise err
    Right x -> return x

-- | Run DB action within Action monad.
runDBA' :: DB a -> Action (Either Error a)
runDBA' qry = do
  pool <- askPoolA
  cfg <- asksConnectionInfo ciGlobalConfig
  logger <- askLogger
  r <- liftIO $ runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) logger
  return r

-- | Run DB action within Daemon monad.
runDB :: DB a -> Daemon (Either Error a)
runDB qry = do
  pool <- askPool
  cfg <- askConfig
  logger <- askLoggerM
  liftIO $ runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) logger

-- | Run DB action within IO monad.
runDBIO :: GlobalConfig -> Sql.ConnectionPool -> Logger -> DB a -> IO (Either Error a)
runDBIO cfg pool logger qry = do
  runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) logger

-- | Run Daemon action within IO monad.
runDaemon :: GlobalConfig -> Maybe Sql.ConnectionPool -> LogBackend -> Daemon a -> IO a
runDaemon cfg mbPool backend daemon =
    runner $ withLogging backend runner (runDaemonT daemon)
  where
    runner r = evalStateT r initState
    initState = ConnectionInfo cfg mbPool

wrapDaemon :: ((c -> IO a) -> IO a) -> (c -> Daemon a) -> Daemon a
wrapDaemon wrapper daemon = do
    cfg <- askConfig
    pool <- askPool
    logger <- askLoggerM
    let connInfo = ConnectionInfo cfg (Just pool)
    result <- liftIO $ wrapper $ \c -> runDaemonIO connInfo logger (daemon c)
    return result

-- | forkIO for Daemon monad.
forkDaemon :: Daemon a -> Daemon ()
forkDaemon daemon = do
    cfg <- askConfig
    pool <- askPool
    logger <- askLoggerM
    let connInfo = ConnectionInfo cfg (Just pool)
    liftIO $ forkIO $ do
               runDaemonIO connInfo logger daemon
               return ()
    return ()

