{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Types where

import qualified Data.Map as M
import Data.Generics

import Schedule

data ParamType =
    StringParam
  | IntParam
  | FileParam
  deriving (Eq, Show, Data, Typeable)

data Job = Job {
    jobTypeName :: String
  , jobParams :: M.Map String String
  }
  deriving (Eq, Show, Data, Typeable)

data Queue = Queue {
    queueName :: String
  , queueSchedule :: Schedule
  , queueJobs :: [Job]
  }
  deriving (Eq, Show, Data, Typeable)

data JobType = JobType {
    jtName :: String
  , jtTemplate :: String
  , jtParams :: M.Map String ParamType
  }
  deriving (Eq, Show, Data, Typeable)

data Error =
    QueueExists
  | QueueNotExists
  | QueueNotEmpty
  deriving (Eq, Show, Data, Typeable)

type Result a = Either Error a

throwR :: forall m a. (Monad m) => Error -> m (Result a)
throwR ex = return $ Left ex

done :: Monad m => m (Result ())
done = return $ Right ()


