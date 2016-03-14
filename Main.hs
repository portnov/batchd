
import Control.Monad
import qualified Data.Map as M
import Data.Acid
import System.Environment

import Database
import Types
import Schedule

main :: IO ()
main = do
  args <- getArgs
  db <- openLocalStateFrom "data/" (Database [])
  case args of
    ["dump"] -> do
        qs <- query db GetAllQueues
        forM_ qs print
    ["create", name] -> do
        r <- update db $ AddQueue $ Queue name anytime []
        case r of
          Right _ -> putStrLn "ok"
          Left err -> print err
    ["enqueue", qname, jobtype] -> do
        r <- update db $ Enqueue qname $ Job jobtype M.empty
        case r of
          Right _ -> putStrLn "ok"
          Left err -> print err

