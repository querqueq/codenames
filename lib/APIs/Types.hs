{-# LANGUAGE DataKinds #-}      
{-# LANGUAGE TypeOperators #-}

module APIs.Types where

import Models.Lobby
import qualified Models.Game as Game
import Servant.API 

type Id = String
type GameId = Capture "gameid" Id
type LobbyId = Capture "lobbyid" Id
type Token = String
type PlayerToken = Header "UserToken" Token
type Team = Capture "team" Game.Team
type PlayerName = Capture "name" Name
