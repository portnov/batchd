
module System.Batchd.AWS
  (
    AWSEC2 (..)
  ) where

import Control.Monad (when)
import Control.Monad.Trans.AWS
import Control.Lens
import qualified Data.Text as T
import Network.AWS.EC2
import Network.AWS.Waiter
import System.Log.Heavy
import System.Log.Heavy.AWS
import System.Batchd

data AWSEC2 = AWSEC2 {
    awsEnableStartStop :: Bool
  , awsCredentials :: Credentials
  , awsRegion :: Region
  , awsLogger :: SpecializedLogger
  }

describe instanceId =
  describeInstances & (diiInstanceIds .~ [instanceId])

instance HostController AWSEC2 where
  doesSupportStartStop aws = awsEnableStartStop aws

  startHost aws name = do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ awsLogger aws)
      let instanceId = T.pack name

      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
              send $ startInstances & (sInstanceIds .~ [instanceId])
              await instanceRunning $ describe instanceId
      when (rs /= AcceptSuccess) $ do
        fail $ "Cannot start instance: " ++ show rs

  stopHost aws name = do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ awsLogger aws)
      let instanceId = T.pack name
      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
                send $ stopInstances & (siInstanceIds .~ [instanceId])
                await instanceStopped $ describe instanceId
      when (rs /= AcceptSuccess) $ do
        fail $ "Cannot stop instance: " ++ show rs

