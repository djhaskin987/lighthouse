{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module LighthouseApp (app) where

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import           Lighthouse
import           Web.Spock
import           Web.Spock.Config
import Network.Wai (Middleware)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

instance (FromJSON w,
          FromJSON r,
          FromJSONKey r,
          Ord r,
          FromJSON n,
          FromJSON g,
          FromJSONKey g,
          Ord g)
  => FromJSON (Workload w r n g) where
  parseJSON = withObject "Workload" $ \o -> do
    wid <- o .: "id"
    reqs <- o .: "requirements"
    tols <- o .:? "tolerations" .!= ([] :: [r])
    avrs <- o .:? "aversion_groups" .!= ([] :: [g])
    return $ Workload wid reqs (Set.fromList tols) (Set.fromList avrs)

instance (ToJSON w,
          ToJSON r,
          ToJSONKey r,
          Ord r,
          ToJSON n,
          ToJSON g)
  => ToJSON (Workload w r n g) where
  toJSON (Workload wid reqs tols avrs) =
    object [
      "id" .= wid,
      "requirements" .= reqs,
      "tolerations" .= Set.toList tols,
      "aversion_groups" .= Set.toList avrs
    ]

instance (FromJSON i,
          FromJSON w,
          FromJSONKey w,
          Ord w,
          FromJSON r,
          FromJSONKey r,
          Ord r,
          FromJSON n,
          Num n,
          Ord n,
          FromJSON g,
          FromJSONKey g,
          Ord g)
  => FromJSON (Node i w r n g) where
  parseJSON = withObject "Node" $ \o -> do
    lid <- o .: "id"
    res <- o .: "resources"
    lds <- o .:? "workloads" .!= (Map.empty :: Map.Map w (Workload w r n g))
    return $ Node lid res lds

instance (ToJSON i,
          ToJSON w,
          ToJSONKey w,
          Ord w,
          ToJSON r,
          ToJSONKey r,
          Ord r,
          ToJSON n,
          ToJSONKey g,
          ToJSON g)
  => ToJSON (Node i w r n g) where
  toJSON (Node lid res lds) =
    object [
      "id" .= lid,
      "resources" .= res,
      "workloads" .= lds
    ]

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

app :: IO Middleware
app = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  spock spockCfg routes

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

routes :: Api
routes =
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
