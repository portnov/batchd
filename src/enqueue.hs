{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Aeson
import qualified Data.Map as M
import Data.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Console.CmdArgs
import System.FilePath

import CommonTypes

data Enqueue = Enqueue {
    managerUrl :: String,
    queueName :: String,
    typeName :: String,
    hostName :: Maybe String,
    commandName :: String,
    commandParam :: String,
    parameters :: [String]
  }
  deriving (Show, Data, Typeable)

defaultUrl :: String
defaultUrl = "http://localhost:3000" 

defaultQueue :: String
defaultQueue = "default"

defaultType :: String
defaultType = "command"

enqueue :: Enqueue
enqueue = Enqueue {
    managerUrl = defaultUrl &= name "url" &= typ defaultUrl &= help "batchd manager API URL",
    queueName = defaultQueue &= name "queue" &= typ "QUEUE" &= help "queue name",
    typeName = defaultType &= name "type" &= typ "TYPE" &= help "job type name",
    hostName = def &= name "host" &= typ "HOST" &= help "worker host name",
    commandName = defaultType &= typ "PARAMNAME" &= help "name of `command` parameter for job",
    commandParam = def &= typ "COMMAND" &= argPos 0,
    parameters = def &= typ "NAME=VALUE" &= help "job parameter"
  }

parseParams :: Enqueue -> JobParamInfo
parseParams e = 
    let pairs = map parseOne (parameters e)
    in  M.insert (commandName e) (commandParam e) $ M.fromList pairs
  where
    parseOne :: String -> (String, String)
    parseOne s = case break (== '=') s of
                   (key, (_:value)) -> (key, value)
                   (key, []) -> (key, "")

testJob :: JobInfo
testJob = JobInfo {
    jiId = 0,
    jiQueue = "",
    jiType = "type1",
    jiSeq = 0,
    jiStatus = New,
    jiTryCount = 0,
    jiHostName = Nothing,
    jiParams = M.fromList [("key", "my value")]
  }

main :: IO ()
main = do
  opts <- cmdArgs enqueue

  let job = JobInfo {
      jiId = 0,
      jiQueue = queueName opts,
      jiType = typeName opts,
      jiSeq = 0,
      jiStatus = New,
      jiTryCount = 0,
      jiHostName = hostName opts,
      jiParams = parseParams opts
    }
  print job

  manager <- newManager defaultManagerSettings

  url <- parseUrl $ managerUrl opts </> "queue" </> queueName opts
  let request = url {
                  method="PUT",
                  requestBody = RequestBodyLBS $ encode job
                }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
