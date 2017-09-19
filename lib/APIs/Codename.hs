{-# LANGUAGE DataKinds #-}      
{-# LANGUAGE TypeOperators #-}

module APIs.Codename where

import APIs.Lobby
import APIs.Game
import Servant.API 

type CodenameAPI = LobbyAPI :<|> GameAPI :<|> Raw
