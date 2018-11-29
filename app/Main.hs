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

data AssignmentStrategy =
    Prioritized
  | RoundRobin deriving (Show, Eq, Generic)

instance ToJSON AssignmentStrategy
instance FromJSON AssignmentStrategy

data AssignWorkloadsArgs =
  AssignWorkloadsArgs { assignmentStrategy :: AssignmentStrategy
                      , nodesArgs :: [Node Text Text Text Float]
                      , workloadsArgs :: [Workload Text Text Float]
                      } deriving (Show, Eq, Generic)

instance ToJSON AssignWorkloadsArgs
instance FromJSON AssignWorkloadsArgs

data AssignWorkloadsResults =
  AssignWorkloadsResults { successful :: Bool
                         , assignments :: Map.Map Text Text
                         } deriving (Show, Eq, Generic)

instance   ToJSON AssignWorkloadsResults
instance FromJSON AssignWorkloadsResults

assignmentsGiven :: AssignWorkloadsArgs -> Maybe (Map.Map Text Text)
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
    prResMgr = (ResourceManager (fromListPR nodes) Map.empty)
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
