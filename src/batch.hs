{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import Data.Aeson
import qualified Data.Map as M
import Data.Maybe
import Data.Char (toLower)
import Data.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Console.CmdArgs
import System.FilePath
import Text.Printf

import CommonTypes
import qualified Database

data CrudMode =
    Add
  | Update
  | Delete
  deriving (Show, Data, Typeable)

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
  | Queue {
      managerUrl :: String,
      queueMode :: CrudMode,
      queueName :: String,
      scheduleName :: Maybe String,
      hostName :: Maybe String,
      force :: Bool
    }
  | Stats {
      managerUrl :: String,
      queueToStat :: [String]
    }
  deriving (Show, Data, Typeable)

defaultUrl :: String
defaultUrl = "http://localhost:" ++ show defaultManagerPort

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
  } &= help "put a new job into queue"

list :: Batch
list = List {
    managerUrl = managerUrlAnn defaultUrl,
    status = def &= typ "STATUS" &= help "list only jobs of specified status",
    queueToList = def &= args &= typ "QUEUE"
  } &= help "list queues or jobs"

queue :: Batch
queue = Queue {
    managerUrl = managerUrlAnn defaultUrl,
    queueMode = enum [
                  Add &= help "create new queue",
                  Update &= help "modify queue",
                  Delete &= help "delete queue"],
    queueName = defaultQueue &= argPos 0 &= typ "QUEUE",
    scheduleName = def &= typ "SCHEDULE" &= help "queue schedule name",
    hostName = Nothing &= name "host" &= typ "HOST" &= help "default host name for queue",
    force = False &= help "force non-empty queue deletion"
  } &= help "create, update or delete queues"
    
stats :: Batch
stats = Stats {
    managerUrl = managerUrlAnn defaultUrl,
    queueToStat = [] &= typ "QUEUE" &= args
  } &= help "print statistics on queue or on all jobs"

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
            printf "%s:\t%s\t%s\n"
                   (Database.queueName queue)
                   (Database.queueScheduleName queue)
                   (fromMaybe "*" $ Database.queueHostName queue)

    qnames ->
      forM_ qnames $ \qname -> do
        statusOpt <- parseStatus (Just New) (fail "Invalid status") (status opts)
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

doStats :: Manager -> Batch -> IO ()
doStats manager opts = do
    case queueToStat opts of
      [] -> do
        let urlStr = managerUrl opts </> "stats"
        putStrLn $ "Querying " ++ urlStr
        url <- parseUrl urlStr
        responseLbs <- httpLbs url manager
        case eitherDecode (responseBody responseLbs) of
          Left err -> fail $ show err
          Right response -> do
            forM_ (M.assocs response) $ \rec -> do
              let qname = fst rec :: String
                  stat = snd rec :: M.Map JobStatus Int
              putStrLn $ qname ++ ":"
              printStats stat
      qnames -> do
        forM_ qnames $ \qname -> do
          putStrLn $ qname ++ ":"
          let urlStr = managerUrl opts </> "stats" </> qname
          -- putStrLn $ "Querying " ++ urlStr
          url <- parseUrl urlStr
          responseLbs <- httpLbs url manager
          case eitherDecode (responseBody responseLbs) of
            Left err -> fail $ show err
            Right response -> do
                printStats response
  where
    printStats :: M.Map JobStatus Int -> IO ()
    printStats stat =
      forM_ (M.assocs stat) $ \(st, cnt) ->
          printf "\t%s:\t%d\n" (show st) cnt

addQueue :: Manager -> Batch -> IO ()
addQueue manager opts = do
  -- let host = if hostName opts == Nothing then Nothing else hostName opts
  let queue = Database.Queue {
                Database.queueName = queueName opts,
                Database.queueScheduleName = fromMaybe "anytime" (scheduleName opts),
                Database.queueHostName = hostName opts
              }
  url <- parseUrl $ managerUrl opts </> "queue"
  let request = url {
                  method="PUT",
                  requestBody = RequestBodyLBS $ encode queue
                }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response

updateQueue :: Manager -> Batch -> IO ()
updateQueue manager opts = do
    let queue = object $
                  toList "schedule_name" (scheduleName opts) ++
                  toList "host_name" (hostName opts)
    url <- parseUrl $ managerUrl opts </> "queue" </> queueName opts
    let request = url {
                    method="POST",
                    requestBody = RequestBodyLBS $ encode queue
                  }
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response
  where
    toList _ Nothing = []
    toList name (Just str) = [name .= str]

deleteQueue :: Manager -> Batch -> IO ()
deleteQueue manager opts = do
  let forceStr = if force opts then "?forced=true" else ""
  url <- parseUrl $ managerUrl opts </> "queue" </> queueName opts ++ forceStr
  let request = url {
                    method="DELETE"
                  }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response

main :: IO ()
main = do
  let mode = cmdArgsMode $ modes [enqueue, list &= name "ls", queue, stats]
  opts <- cmdArgsRun mode

  manager <- newManager defaultManagerSettings

  case opts of
    Enqueue {} -> doEnqueue manager opts
    List {} -> doList manager opts
    Stats {} -> doStats manager opts
    Queue {} ->
      case queueMode opts of
        Add -> addQueue manager opts
        Update -> updateQueue manager opts
        Delete -> deleteQueue manager opts

