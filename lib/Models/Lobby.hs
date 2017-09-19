{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Models.Lobby where

import Prelude hiding (Word)
import GHC.Generics
import Data.Aeson
import Models.Game
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type LobbyUpdate = Either LobbyError (Lobby, [LobbyEvent])
type Name = String
data GameConfig = GameConfig
    { numberOfTeams :: Int
    } deriving (Generic, Show, ToJSON, FromJSON)

data Members = Members
    { spy       :: Maybe Name
    , spymaster :: Maybe Name
    } deriving (Show, Generic, ToJSON, FromJSON)

data Lobby = Lobby
    { unassignedPlayers :: [Name]
    , assignedPlayers   :: Map Team Members
    } deriving (Generic, Show, ToJSON, FromJSON)

data LobbyEvent = PlayerJoined Name | PlayerLeft Name | TeamAssigned Team Name 
                | TeamUnassigned Name | RolesSwitched Members | Closed | Starting
    deriving (Generic, Show, ToJSON, FromJSON)
data LobbyError = LobbyFull | DuplicateName Name | UnknownPlayer Name | UnknownTeam Team | TeamFull
    deriving (Generic, Show)

exampleLobby :: Lobby
exampleLobby = Lobby ["john", "quentin"] $ Map.fromList [(1,Members (Just "jim") Nothing),(2,Members Nothing Nothing)]
