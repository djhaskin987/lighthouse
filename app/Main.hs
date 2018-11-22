{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import           Lighthouse

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8087 (spock spockCfg app)


data AssignWorkloadsArgs =
  AssignWorkloadsArgs { nodesArgs :: [Node]
                      , workloadsArgs :: [Workload]
                      } deriving (Show, Eq, Generic)

instance ToJSON AssignWorkloadsArgs
instance FromJSON AssignWorkloadsArgs

data AssignWorkloadsResults =
  AssignWorkloadsResults { successful :: Bool
                         , assignments :: [Assignment]
                         } deriving (Show, Eq, Generic)

instance   ToJSON AssignWorkloadsResults
instance FromJSON AssignWorkloadsResults

app :: Api
app = do
  post "assign-workloads" $ do
    rpcArgs <- jsonBody' :: ApiAction AssignWorkloadsArgs
    case assignWorkloads (ResourceManager (nodesArgs rpcArgs) []) (workloadsArgs rpcArgs) of
      Nothing -> json $
        AssignWorkloadsResults {
          successful = False,
          assignments = []
        }
      Just (ResourceManager nodes asgn) -> json $
        AssignWorkloadsResults {
          successful = True,
          assignments = asgn
        }
