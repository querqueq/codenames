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

type Id = String
type GameId = Capture "gameid" Id
type LobbyId = Capture "lobbyid" Id
type Token = String
type PlayerToken = Header "UserToken" Token
type Team = Capture "team" Game.Team
type PlayerName = Capture "name" Name

type CodenameAPI = LobbyAPI :<|> GameAPI

type LobbyAPI = "lobbies" :> Get '[JSON] [(Id,Lobby)]
              -- Creates a new lobby and returns its game id
              :<|> "lobbies" :> ReqBody '[JSON] GameConfig :> Post '[JSON] (Id, Lobby)
              -- Requests for a specific lobby
              :<|> "lobbies" :> LobbyId :> (
                -- Returns the current state of the lobby
                     Get '[JSON] Lobby
                -- Joins a game if its neither started nor full and returns a secret player token
                :<|> "players" :> ReqBody '[JSON] Name :> Post '[JSON] Token
                -- Requests from a specific player
                :<|> PlayerToken :> (
                        -- Leave a game
                        "players" :> PlayerName :> DeleteNoContent '[JSON] NoContent
                        -- Join a team
                   :<|> "teams" :> Team :> ReqBody '[JSON] Name :> Post '[JSON] NoContent
                        -- Leave a team
                   :<|> "teams" :> Team :> "players" :> PlayerName :> Delete '[JSON] NoContent
                        -- Switch role
                   :<|> "teams" :> Team :> "switch" :> Post '[JSON] NoContent
                   )
                )
                      
          -- Start a game
type GameAPI = "games" :> ReqBody '[JSON] (Token, LobbyId) :> Post '[JSON] GameId
          -- Returns the current state of the game from the view of a spy
          :<|> "games" :> GameId :> Get '[JSON] Game
          -- Returns the role and team of a player
          :<|> "games" :> GameId :> "players" :> PlayerToken :> Get '[JSON] Role  
          -- Submits a move and returns nothing if it was a legal move otherwise returns an illegal change
          :<|> "games" :> GameId :> "move" :> ReqBody '[JSON] (Token, Move) :> Post '[JSON] (Maybe IllegalChange)
