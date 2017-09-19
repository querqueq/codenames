{-# LANGUAGE DataKinds #-}      
{-# LANGUAGE TypeOperators #-}

module APIs.Game where

import APIs.Types
import Models.Game
import Servant.API 

          -- Start a game
type GameAPI = "games" :> ReqBody '[JSON] (Token, LobbyId) :> Post '[JSON] GameId
          -- Returns the current state of the game from the view of a spy
          :<|> "games" :> GameId :> Get '[JSON] Game
          -- Returns the role and team of a player
          :<|> "games" :> GameId :> "players" :> PlayerToken :> Get '[JSON] Role  
          -- Submits a move and returns nothing if it was a legal move otherwise returns an illegal change
          :<|> "games" :> GameId :> "move" :> ReqBody '[JSON] (Token, Move) :> Post '[JSON] (Maybe IllegalChange)
