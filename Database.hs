{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module Database where

import Data.Acid

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import System.Environment                    ( getArgs )
import Data.SafeCopy
import Data.Generics
import Data.List
import Data.Dates

import Types
import Schedule

data Database = Database [Queue]
  deriving (Eq, Show, Data, Typeable)

deriveSafeCopy 0 'base ''Error
deriveSafeCopy 0 'base ''Time
deriveSafeCopy 0 'base ''WeekDay
deriveSafeCopy 0 'base ''TimePeriod
deriveSafeCopy 0 'base ''Schedule
deriveSafeCopy 0 'base ''Job
deriveSafeCopy 0 'base ''Queue
deriveSafeCopy 0 'base ''Database

addQueue :: Queue -> Update Database (Result ())
addQueue q = do
  Database qs <- get
  let existingNames = map queueName qs
  if queueName q `elem` existingNames
    then throwR QueueExists
    else do
         put $ Database $ q : qs
         done

getQueue :: String -> Query Database (Maybe Queue)
getQueue name = do
  Database qs <- ask
  let lst = [q | q <- qs, queueName q == name]
  case lst of
    (q:_) -> return $ Just q
    _ -> return Nothing

getAllQueues :: Query Database [Queue]
getAllQueues = do
  Database qs <- ask
  return qs

deleteQueue :: String -> Bool -> Update Database (Result ())
deleteQueue name forced = do
  Database qs <- get
  let existingNames = map queueName qs
  if name `elem` existingNames
    then do
         let ([toDelete], other) = partition (\q -> queueName q == name) qs
         if null (queueJobs toDelete) || forced
           then do
                put $ Database other
                done
           else throwR QueueNotEmpty
    else throwR QueueNotExists
  
updateQueue :: String -> (Queue -> Queue) -> Update Database (Result ())
updateQueue name fn = do
  Database qs <- get
  let (that, other) = partition (\q -> queueName q == name) qs
  case that of
    (q:_) -> do
             put $ Database $ fn q : other
             done
    _ -> throwR QueueNotExists

enqueue :: String -> Job -> Update Database (Result ())
enqueue name job = updateQueue name (\q -> q {queueJobs = queueJobs q ++ [job]})

makeAcidic ''Database ['addQueue, 'getQueue, 'deleteQueue, 'getAllQueues, 'enqueue]

