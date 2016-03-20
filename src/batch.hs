{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import Data.Aeson
import qualified Data.Map as M
import Data.Char (toLower)
import Data.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Console.CmdArgs
import System.FilePath
import Text.Printf

import CommonTypes
import qualified Database

data Batch =
    Enqueue {
      managerUrl :: String,
      queueName :: String,
      typeName :: String,
      hostName :: Maybe String,
      commandName :: String,
      commandParam :: String,
      parameters :: [String]
    }
  | List {
      managerUrl :: String,
      status :: Maybe String,
      queueToList :: [String]
    }
  deriving (Show, Data, Typeable)

defaultUrl :: String
defaultUrl = "http://localhost:3000" 

defaultQueue :: String
defaultQueue = "default"

defaultType :: String
defaultType = "command"

managerUrlAnn url = url &= name "url" &= typ defaultUrl &= help "batchd manager API URL"

enqueue :: Batch
enqueue = Enqueue {
    managerUrl = managerUrlAnn defaultUrl,
    queueName = defaultQueue &= name "queue" &= typ "QUEUE" &= help "queue name",
    typeName = defaultType &= name "type" &= typ "TYPE" &= help "job type name",
    hostName = def &= name "host" &= typ "HOST" &= help "worker host name",
    commandName = defaultType &= typ "PARAMNAME" &= help "name of `command` parameter for job",
    commandParam = def &= typ "COMMAND" &= argPos 0,
    parameters = def &= typ "NAME=VALUE" &= help "job parameter"
  }

list :: Batch
list = List {
    managerUrl = managerUrlAnn defaultUrl,
    status = def &= typ "STATUS" &= help "list only jobs of specified status",
    queueToList = def &= args &= typ "QUEUE"
  }

parseParams :: Batch -> JobParamInfo
parseParams e = 
    let pairs = map parseOne (parameters e)
    in  M.insert (commandName e) (commandParam e) $ M.fromList pairs
  where
    parseOne :: String -> (String, String)
    parseOne s = case break (== '=') s of
                   (key, (_:value)) -> (key, value)
                   (key, []) -> (key, "")

doEnqueue :: Manager -> Batch -> IO ()
doEnqueue manager opts = do
  let qname = queueName opts

  let job = JobInfo {
      jiId = 0,
      jiQueue = qname,
      jiType = typeName opts,
      jiSeq = 0,
      jiStatus = New,
      jiTryCount = 0,
      jiHostName = hostName opts,
      jiParams = parseParams opts
    }
  print job


  url <- parseUrl $ managerUrl opts </> "queue" </> qname
  let request = url {
                  method="PUT",
                  requestBody = RequestBodyLBS $ encode job
                }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response

doList :: Manager -> Batch -> IO ()
doList manager opts = do
  case queueToList opts of
    [] -> do
      let urlStr = managerUrl opts </> "queue"
      putStrLn $ "Querying " ++ urlStr
      url <- parseUrl urlStr
      responseLbs <- httpLbs url manager
      case eitherDecode (responseBody responseLbs) of
        Left err -> fail $ show err
        Right response -> do
          forM_ response $ \queue -> do
            printf "%s:\t%s\n" (Database.queueName queue) (Database.queueScheduleName queue)

    qnames ->
      forM_ qnames $ \qname -> do
        print (status opts)
        statusOpt <- parseStatus Nothing (fail "Invalid status") (status opts)
        let statusStr = case statusOpt of
                          Nothing -> "?status=all"
                          Just st -> case status opts of
                                       Nothing -> ""
                                       _ -> "?status=" ++ map toLower (show st)
        let urlStr = managerUrl opts </> "queue" </> qname ++ statusStr
        putStrLn $ "Querying " ++ urlStr
        url <- parseUrl urlStr
        responseLbs <- httpLbs url manager
        case eitherDecode (responseBody responseLbs) of
          Left err -> fail $ show err
          Right response -> do
            forM_ response $ \job -> do
              printf "#%d: [%d]\t%s\t%s\n" (jiId job) (jiSeq job) (jiType job) (show $ jiStatus job)
              forM_ (M.assocs $ jiParams job) $ \(name, value) -> do
                printf "\t%s:\t%s\n" name value

main :: IO ()
main = do
  let mode = cmdArgsMode $ modes [enqueue, list]
  opts <- cmdArgsRun mode

  manager <- newManager defaultManagerSettings

  case opts of
    Enqueue {} -> doEnqueue manager opts
    List {} -> doList manager opts

