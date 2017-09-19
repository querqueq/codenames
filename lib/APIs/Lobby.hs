{-# LANGUAGE DataKinds #-}      
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- {-# LANGUAGE PolyKinds #-}      
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module APIs.Lobby where

import Models.Lobby
import Servant.API 
import APIs.Types

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
