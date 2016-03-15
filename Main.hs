{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Map as M
import Database.Persist
import Database.Persist.Sqlite
import System.Environment
import Text.Printf

import Database
import Types
import Schedule

main :: IO ()
main = do
  args <- getArgs
  runSqlite "test.db" $ do
    runMigration migrateAll

    -- dbio $ addSchedule anytime >> return ()

    case args of
      ["create", name] -> do
        let Right one = keyFromValues [PersistInt64 1]
        dbio $ void $ addQueue name one

      ["enqueue", qname, jtype] -> do
        let Right zero = keyFromValues [PersistInt64 0]
        let jinfo = JobInfo (Job jtype zero 0) []
        dbio $ void $ enqueue qname jinfo

      ["dump"] -> dbio $ do
        qes <- getAllQueues
        forM_ qes $ \qe -> do
          let qname = queueName $ entityVal qe
          liftIO $ putStrLn $ qname ++ ":"
          jes <- getAllJobs (entityKey qe)
          forM_ jes $ \je -> do
            let job = entityVal je
                jid = show (entityKey je)
            liftIO $ printf "  %d: %s) %s\n" (jobSeq job) jid (jobType job)

