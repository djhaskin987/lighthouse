{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import           Lighthouse
import           Web.Spock
import           Web.Spock.Config
import qualified Data.Map.Strict as Map

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
                         , assignments :: Map.Map WorkloadID NodeID
                         } deriving (Show, Eq, Generic)

instance   ToJSON AssignWorkloadsResults
instance FromJSON AssignWorkloadsResults

app :: Api
app = do
  post "assign-workloads" $ do
    rpcArgs <- jsonBody' :: ApiAction AssignWorkloadsArgs
    case assignWorkloads (ResourceManager (nodesArgs rpcArgs) Map.empty) (workloadsArgs rpcArgs) of
      Nothing -> json $
        AssignWorkloadsResults {
          successful = False,
          assignments = Map.empty
        }
      Just (ResourceManager nodes asgn) -> json $
        AssignWorkloadsResults {
          successful = True,
          assignments = asgn
        }
