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

instance FromJSON (Workload Text Text Float)
instance   ToJSON (Workload Text Text Float)

instance FromJSON (Node Text Text Text Float)
instance   ToJSON (Node Text Text Text Float)

data AssignmentStrategy = Prioritized | RoundRobin deriving (Show, Eq, Generic)

instance ToJSON AssignmentStrategy
instance FromJSON AssignmentStrategy

data AssignWorkloadsArgs =
  AssignWorkloadsArgs { assignmentStrategy :: AssignmentStrategy
                      , nodesArgs :: [Node]
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

assignmentsGiven :: AssignWorkloadsArgs -> Maybe (Map.Map WorkloadID NodeID)
assignmentsGiven rpcArgs = 
  case (assignmentStrategy rpcArgs) of
    Prioritized -> do
      prTarget <- assignWorkloads prResMgr loads
      return $ mgrAssignments prTarget
    RoundRobin -> do
      rrTarget <- assignWorkloads rrResMgr loads
      return $ mgrAssignments rrTarget
  where
    loads = (workloadsArgs rpcArgs)
    nodes = (nodesArgs rpcArgs)
    prResMgr = (ResourceManager nodes Map.empty)
    rrResMgr = (ResourceManager (fromListRR nodes) Map.empty)

app :: Api
app = do
  post "assign-workloads" $ do
    rpcArgs <- jsonBody' :: ApiAction AssignWorkloadsArgs
    case (assignmentsGiven rpcArgs) of
      Nothing -> json $
        AssignWorkloadsResults {
          successful = False,
          assignments = Map.empty
        }
      Just asgn -> json $
        AssignWorkloadsResults {
          successful = True,
          assignments = asgn
        }
