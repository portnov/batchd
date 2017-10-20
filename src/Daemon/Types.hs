{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveGeneric, StandaloneDeriving, OverloadedStrings, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Daemon.Types where

import Control.Exception (catch)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Control.Concurrent
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F
import qualified Database.Persist.Sql as Sql
import Network.Wai
import qualified Web.Scotty.Trans as Scotty
import qualified Web.Scotty.Internal.Types as SI
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
  lts <- lift $ ask
  x <- liftIO $ runResourceT $ runExceptT $ runLoggingT (runReaderT action backend) lts
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
  deriving (Applicative,Functor,Monad,MonadIO, MonadReader LoggingTState, HasLogContext, HasLogger)

-- | Run daemon actions within IO monad
runDaemonIO' :: ConnectionInfo -> SpecializedLogger -> Daemon a -> IO a
runDaemonIO' connInfo logger actions =
  let globalContext = LogContextFrame [] NoChange
      lts = LoggingTState logger [globalContext]
  in  evalStateT (runLoggingT (runDaemonT actions) lts) connInfo

-- | Run daemon actions within IO monad
runDaemonIO :: ConnectionInfo -> LoggingTState -> Daemon a -> IO a
runDaemonIO connInfo lts actions =
  evalStateT (runLoggingT (runDaemonT actions) lts) connInfo

-- | REST handler monad type
type Action a = Scotty.ActionT Error Daemon a

instance HasLogContext (Scotty.ActionT Error Daemon) where
  getLogContext = lift getLogContext

  withLogContext frame actions =
      SI.ActionT $ ExceptT $ ReaderT $ \env -> StateT $ \rs ->
        withLogContext frame $ runStateT (runReaderT (runExcept $ SI.runAM actions) env) rs
    where
      runExcept (ExceptT m) = m

instance HasLogger (Scotty.ActionT Error Daemon) where
  getLogger = lift getLogger

  localLogger logger actions = 
      SI.ActionT $ ExceptT $ ReaderT $ \env -> StateT $ \rs ->
        localLogger logger $ runStateT (runReaderT (runExcept $ SI.runAM actions) env) rs
    where
      runExcept (ExceptT m) = m

instance F.VarContainer Request where
  lookupVar "method" rq = Just $ F.Variable $ show $ httpVersion rq
  lookupVar "path" rq = Just $ F.Variable $ rawPathInfo rq
  lookupVar "secure" rq = Just $ F.Variable $ isSecure rq
  lookupVar "referer" rq = Just $ F.Variable $ requestHeaderReferer rq
  lookupVar "useragent" rq = Just $ F.Variable $ requestHeaderUserAgent rq
  lookupVar _ rq = Nothing

-- | Obtain DB connection
askConnectionInfo :: Daemon ConnectionInfo
askConnectionInfo = Daemon $ lift get

-- | Obtain field of DB connection within Action monad
asksConnectionInfo :: (ConnectionInfo -> a) -> Action a
asksConnectionInfo fn = do
  ci <- lift askConnectionInfo
  return $ fn ci

-- | Obtain logger function.
-- Note: if logger is called directly, it does not perform
-- context-level filter check.
askLoggerM :: Daemon SpecializedLogger
askLoggerM = asks ltsLogger

-- | Obtain logging state within Daemon monad
askLoggingStateM :: Daemon LoggingTState
askLoggingStateM = ask

-- | Obtain logger function.
-- Note: if logger is called directly, it does not perform
-- context-level filter check.
askLogger :: Action SpecializedLogger
askLogger = lift $ askLoggerM

-- | Obtain logging state within Action monad
askLoggingState :: Action LoggingTState
askLoggingState = lift $ askLoggingStateM

-- | Obtain DB connections pool
askPool :: Daemon Sql.ConnectionPool
askPool = do
  mbPool <- Daemon $ lift $ gets ciPool
  case mbPool of
    Nothing -> fail $ "Database connection is not established yet"
    Just pool -> return pool

-- | Obtain global daemon configuration
askConfig :: Daemon GlobalConfig
askConfig = Daemon $ lift $ gets ciGlobalConfig

-- | Obtain DB connections pool within Action monad
askPoolA :: Action Sql.ConnectionPool
askPoolA = do
  mbPool <- asksConnectionInfo ciPool
  case mbPool of
    Nothing -> fail $ "Database connection is not established yet"
    Just pool -> return pool

-- | Obtain global daemon configuration within Action monad
askConfigA :: Action GlobalConfig
askConfigA = asksConnectionInfo ciGlobalConfig

-- | Run DB action within Action monad. Raise HTTP-level error if DB action fails.
runDBA :: DB a -> Action a
runDBA qry = do
  pool <- askPoolA
  cfg <- askConfigA
  lts <- askLoggingState
  r <- liftIO $ do
         (runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) lts)
          `catch` (\e -> return $ Left $ SqlError e)
  case r of
    Left err -> Scotty.raise err
    Right x -> return x

-- | Run DB action within Action monad.
-- This version does not raise HTTP-level error on DB action fail.
runDBA' :: DB a -> Action (Either Error a)
runDBA' qry = do
  pool <- askPoolA
  cfg <- asksConnectionInfo ciGlobalConfig
  lts <- askLoggingState
  r <- liftIO $ do
         (runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) lts)
          `catch` (\e -> return $ Left $ SqlError e)
  return r

-- | Run DB action within Daemon monad.
runDB :: DB a -> Daemon (Either Error a)
runDB qry = do
  pool <- askPool
  cfg <- askConfig
  lts <- askLoggingStateM
  liftIO $ do
    (runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) lts)
      `catch` (\e -> return $ Left $ SqlError e)

-- | Run DB action within IO monad.
runDBIO :: GlobalConfig -> Sql.ConnectionPool -> LoggingTState -> DB a -> IO (Either Error a)
runDBIO cfg pool lts qry = do
  (runResourceT $ runLoggingT (Sql.runSqlPool (dbio qry) pool) lts)
    `catch` (\e -> return $ Left $ SqlError e)

-- | Run Daemon action within IO monad.
-- This is generally used to spawn main daemon threads.
runDaemon :: GlobalConfig -> Maybe Sql.ConnectionPool -> LoggingSettings -> Daemon a -> IO a
runDaemon cfg mbPool backend daemon =
    runner $ withLoggingT backend $ 
      withLogContext (LogContextFrame [] NoChange) $ runDaemonT daemon
  where
    runner r = evalStateT r initState
    initState = ConnectionInfo cfg mbPool
    -- undefinedLogger = error "Internal error: logger is not defined yet"

-- | Wrap Daemon action with some @with@-style function in IO monad.
-- Example: @wrapDaemon (withResource x) $ \resource -> do ...@.
wrapDaemon :: ((c -> IO a) -> IO a) -> (c -> Daemon a) -> Daemon a
wrapDaemon wrapper daemon = do
    cfg <- askConfig
    pool <- askPool
    lts <- askLoggingStateM
    let connInfo = ConnectionInfo cfg (Just pool)
    result <- liftIO $ wrapper $ \c -> runDaemonIO connInfo lts (daemon c)
    return result

-- | forkIO for Daemon monad.
forkDaemon :: Daemon a -> Daemon ()
forkDaemon daemon = do
    cfg <- askConfig
    pool <- askPool
    lts <- askLoggingStateM
    let connInfo = ConnectionInfo cfg (Just pool)
    liftIO $ forkIO $ do
               runDaemonIO connInfo lts daemon
               return ()
    return ()

