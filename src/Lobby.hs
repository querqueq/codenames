{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Lobby where

import Prelude hiding (Word)
import GHC.Generics
import Data.Aeson
import Game
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map

type LobbyUpdate = Either String (Lobby, [LobbyEvent])
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

data LobbyEvent = PlayerJoined Name | PlayerLeft Name | TeamAssigned Team Name | RolesSwitched Members | Closed | Starting
    deriving (Generic, Show, ToJSON, FromJSON)

teamFull' :: Members -> Bool
teamFull' (Members (Just _) (Just _)) = True
teamFull' _ = False

teamFull :: Lobby -> Team -> Bool
teamFull (Lobby {..}) team = maybe False teamFull' $ Map.lookup team assignedPlayers

lobbyFull :: Lobby -> Bool
lobbyFull l = freeSpots l == 0

freeSpots :: Lobby -> Int
freeSpots (Lobby {..}) = totalSpots - filledSpots - (length unassignedPlayers)
    where filledSpots = Map.foldl (\x (Members {..}) -> justInc spy + justInc spymaster + x) 0 assignedPlayers
          totalSpots = (length assignedPlayers) * 2
          justInc = maybe 0 (const 1)

lobbyReady :: Lobby -> Bool
lobbyReady l@(Lobby {..}) = lobbyFull l && length unassignedPlayers == 0

getTeam :: Lobby -> Name -> Maybe Team
getTeam (Lobby {..}) name = listToMaybe $ Map.keys $ Map.filter checkTeam assignedPlayers
    where checkTeam (Members {..}) = isName spy || isName spymaster
          isName = maybe False (==name) 

playerJoin :: Lobby -> Name -> LobbyUpdate
playerJoin l@(Lobby {..}) name
    | lobbyFull l  = Left "lobby full"
    | False = Left "duplicate name" -- TODO check if name is already in lobby
    | otherwise = Right $ (Lobby (unassignedPlayers ++ [name]) assignedPlayers, [PlayerJoined name])

playerLeave :: Lobby -> Name -> LobbyUpdate
playerLeave l@(Lobby {..}) name =
    case getTeam l name of
        (Just team) -> Right $ (Lobby unassignedPlayers $ Map.adjust (memberLeave name) team assignedPlayers, events)
        Nothing -> if length unassignedPlayers' < length unassignedPlayers 
            then Right $ (Lobby unassignedPlayers' assignedPlayers, events)
            else Left $ "player " ++ name ++ " not in this lobby"
    where memberLeave name (Members {..}) = if spy == (Just name) then Members Nothing spymaster else Members spy Nothing
          events = [PlayerLeft name]
          unassignedPlayers' = filter (/=name) unassignedPlayers

assignTeam :: Lobby -> Team -> Name -> LobbyUpdate
assignTeam l team name
    | teamFull l team = Left $ "team is full"
    | otherwise = (\(Lobby {..},_) -> (Lobby unassignedPlayers $ Map.adjust join team assignedPlayers, [TeamAssigned team name])) <$> playerLeave l name
    where join (Members Nothing x) = Members (Just name) x
          join (Members x Nothing) = Members x (Just name)

switchRoles :: Lobby -> Team -> LobbyUpdate
switchRoles l@(Lobby {..}) team = 
    case Map.lookup team assignedPlayers of
        Nothing -> Left $ "team " ++ show team ++ "not found"
        (Just (Members {..})) -> let
                    roleSwitch = Members spymaster spy
                    events (Members Nothing Nothing) = []
                    events m = [RolesSwitched m]
                in Right (Lobby unassignedPlayers $ Map.insert team roleSwitch assignedPlayers, events roleSwitch)

startGame :: [Word] -> Lobby -> LobbyUpdate
startGame words l@(Lobby {..}) = undefined

exampleLobby = Lobby ["john", "quentin"] $ Map.fromList [(1,Members (Just "jim") Nothing),(2,Members Nothing Nothing)]
