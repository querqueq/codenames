{-# LANGUAGE RecordWildCards #-}

module Logic.Lobby where

import Prelude hiding (Word)
import GHC.Generics
import Data.Aeson
import Models.Game
import Models.Lobby
import Data.Map.Strict (Map)
import Data.Maybe
import qualified Data.Map.Strict as Map

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

getTeam :: Name -> Lobby -> Maybe Team
getTeam name (Lobby {..}) = listToMaybe $ Map.keys $ Map.filter checkTeam assignedPlayers
    where checkTeam (Members {..}) = isName spy || isName spymaster
          isName = maybe False (==name) 

playerJoin :: Name -> Lobby -> LobbyUpdate
playerJoin name l@(Lobby {..})
    | lobbyFull l  = Left LobbyFull
    | playerInLobby name l = Left $ DuplicateName name 
    | otherwise = Right $ (Lobby (unassignedPlayers ++ [name]) assignedPlayers, [PlayerJoined name])

playerInLobby :: Name -> Lobby -> Bool
playerInLobby name l@(Lobby {..}) = maybe (elem name unassignedPlayers) (const True) (getTeam name l) 

playerLeave :: Name -> Lobby -> LobbyUpdate
playerLeave name l@(Lobby {..}) =
    case getTeam name l of
        (Just team) -> Right $ (Lobby unassignedPlayers $ Map.adjust (memberLeave name) team assignedPlayers, events)
        Nothing -> if length unassignedPlayers' < length unassignedPlayers 
            then Right $ (Lobby unassignedPlayers' assignedPlayers, events)
            else Left $ UnknownPlayer name
    where memberLeave name (Members {..}) = if spy == (Just name) then Members Nothing spymaster else Members spy Nothing
          events = [PlayerLeft name]
          unassignedPlayers' = filter (/=name) unassignedPlayers

assignTeam :: Team -> Name -> Lobby -> LobbyUpdate
assignTeam team name l
    | teamFull l team = Left TeamFull
    | otherwise = (\(Lobby {..},_) -> (Lobby unassignedPlayers $ Map.adjust join team assignedPlayers, [TeamAssigned team name])) <$> playerLeave name l
    where join (Members Nothing x) = Members (Just name) x
          join (Members x Nothing) = Members x (Just name)

unassignTeam :: Name -> Lobby -> LobbyUpdate
unassignTeam name l = (\(Lobby {..},_) -> (Lobby (unassignedPlayers ++ [name]) assignedPlayers, [TeamUnassigned name])) <$> playerLeave name l

switchRoles :: Team -> Lobby -> LobbyUpdate
switchRoles team l@(Lobby {..}) = 
    case Map.lookup team assignedPlayers of
        Nothing -> Left $ UnknownTeam team
        (Just Members {..}) -> let
                    roleSwitch = Members spymaster spy
                    events (Members Nothing Nothing) = []
                    events m = [RolesSwitched m]
                in Right (Lobby unassignedPlayers $ Map.insert team roleSwitch assignedPlayers, events roleSwitch)

startGame :: [Word] -> Lobby -> LobbyUpdate
startGame words l@Lobby {..} = undefined

exampleLobby = Lobby ["john", "quentin"] $ Map.fromList [(1,Members (Just "jim") Nothing),(2,Members Nothing Nothing)]
