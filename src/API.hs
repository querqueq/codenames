{-# LANGUAGE DataKinds #-}      
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- {-# LANGUAGE PolyKinds #-}      
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import Lobby
import Game hiding (Team)
import qualified Game as Game
import Servant.API 

type Id = Int
type GameId = Capture "gameid" Id
type LobbyId = Capture "lobbyid" Id
type Token = String
type PlayerToken = Capture "player" Token
type Team = Capture "team" Game.Team

type CodenameAPI = LobbyAPI :<|> GameAPI

type LobbyAPI = "lobbies" :> Get '[JSON] [(Id,Lobby)]
              -- Creates a new lobby and returns its game id
              :<|> "lobbies" :> ReqBody '[JSON] GameConfig :> Post '[JSON] Lobby
              -- Returns the current state of the lobby
              :<|> "lobbies" :> LobbyId :> Get '[JSON] Lobby
              -- Joins a game if its neither started nor full and returns a secret player token
              :<|> "lobbies" :> LobbyId :> "players" :> ReqBody '[JSON] Name :> Post '[JSON] Token
              -- Leave a game
              :<|> "lobbies" :> LobbyId :> "players" :> PlayerToken :> DeleteNoContent '[JSON] NoContent
              -- Join a team
              :<|> "lobbies" :> LobbyId :> "teams" :> Team :> ReqBody '[JSON] Token :> Post '[JSON] NoContent
              -- Leave a team
              :<|> "lobbies" :> LobbyId :> "teams" :> Team :> PlayerToken :> Delete '[JSON] NoContent
              -- Switch role
              :<|> "lobbies" :> LobbyId :> "teams" :> Team :> "switch" :> ReqBody '[JSON] Token :> Post '[JSON] NoContent
              -- Moved directly to full api since enter cannot handle custom monad
--              :<|> "lobbies" :> LobbyId :> "events" :> Raw


          -- Start a game
type GameAPI = "games" :> ReqBody '[JSON] (Token, LobbyId) :> Post '[JSON] GameId
          -- Returns the current state of the game from the view of a spy
          :<|> "games" :> GameId :> Get '[JSON] Game
          -- Returns the role and team of a player
          :<|> "games" :> GameId :> "players" :> PlayerToken :> Get '[JSON] Role  
          -- Submits a move and returns nothing if it was a legal move otherwise returns an illegal change
          :<|> "games" :> GameId :> "move" :> ReqBody '[JSON] (Token, Move) :> Post '[JSON] (Maybe IllegalChange)
