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
  | RoundRobin
  | BinPack deriving (Show, Eq, Generic)


instance ToJSON AssignmentStrategy
instance FromJSON AssignmentStrategy

data AssignWorkloadsArgs =
  AssignWorkloadsArgs { assignmentStrategy :: AssignmentStrategy
                      , nodesArgs :: [Node Text Text Text Float Text]
                      , workloadsArgs :: [Workload Text Text Float Text]
                      , rubricArgs :: Map.Map Text Float
                      } deriving (Show, Eq, Generic)

instance ToJSON AssignWorkloadsArgs where
  toJSON (AssignWorkloadsArgs strat ns ls rub) =
    object [
      "strategy" .= strat,
      "nodes" .= ns,
      "workloads" .= ls,
      "rubric" .= rub
    ]

instance FromJSON AssignWorkloadsArgs where
  parseJSON = withObject "params" $ \o -> do
    aStrat <- o .:? "strategy" .!= Prioritized
    ns <- o .: "nodes"
    wls <- o .: "workloads"
    rub <- o .:? "rubric" .!= (Map.empty :: Map.Map Text Float)
    return $ AssignWorkloadsArgs aStrat ns wls rub

data AssignWorkloadsResults =
  AssignWorkloadsResults { successful :: Bool
                         , assignments :: Map.Map Text Text
                         } deriving (Show, Eq, Generic)

instance   ToJSON AssignWorkloadsResults
instance FromJSON AssignWorkloadsResults

assignmentsGiven :: AssignWorkloadsArgs -> Maybe (Map.Map Text Text)
assignmentsGiven (AssignWorkloadsArgs strat ns ls rub) =
  case strat of
    Prioritized -> do
      prTarget <- assignWorkloads prResMgr ls
      return $ mgrAssignments prTarget
    RoundRobin -> do
      rrTarget <- assignWorkloads rrResMgr ls
      return $ mgrAssignments rrTarget
    BinPack -> do
      rbDistributor <- fromListRB rub ns
      let rbResMgr = ResourceManager rbDistributor Map.empty in do
        rbTarget <- assignWorkloads rbResMgr (sortWorkloads ls rub)
        return $ mgrAssignments rbTarget
  where
    prResMgr = ResourceManager (fromListPR ns) Map.empty
    rrResMgr = ResourceManager (fromListRR ns) Map.empty

app :: Api
app =
  post "assign-workloads" $ do
    rpcArgs <- jsonBody' :: ApiAction AssignWorkloadsArgs
    case assignmentsGiven rpcArgs of
      Nothing -> json
        AssignWorkloadsResults {
          successful = False,
          assignments = Map.empty
        }
      Just asgn -> json
        AssignWorkloadsResults {
          successful = True,
          assignments = asgn
        }
