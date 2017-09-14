{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Models where

import GHC.Generics
import Data.Aeson
import Data.Map.Strict (Map)
import GameModels

type Name = String
data GameConfig = GameConfig
    { numberOfTeams :: Int
    } deriving (Generic, Show)

data Members = Members
    { spy       :: Maybe Name
    , spymaster :: Maybe Name
    } deriving (Show, Generic)

data Lobby = Lobby
    { unassignedPlayer  :: [Name]
    , assignedPlayers   :: Map Team Members
    } deriving (Generic, Show)

data LobbyEvent = Joined Name | Left Name | TeamAssigned Name | RoleAssigned Name | Closed | Starting
    deriving (Generic, Show)

instance ToJSON GameConfig
instance FromJSON GameConfig
instance ToJSON Lobby
instance FromJSON Lobby
instance ToJSON Members
instance FromJSON Members
instance ToJSON LobbyEvent
instance FromJSON LobbyEvent
