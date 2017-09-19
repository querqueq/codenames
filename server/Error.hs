{-# LANGUAGE OverloadedStrings #-}

module Error where

import Models.Lobby
import Classes
import Servant
import Data.ByteString.Lazy.Char8 (pack)

instance ToServantErr LobbyError where
    toServantErr LobbyFull = err409 { errBody = "lobby already full" }
    toServantErr (DuplicateName name) = err409 { errBody = (pack $ "player " ++ name ++ " already in lobby") }
    toServantErr (UnknownPlayer name) = err404 { errBody = (pack $ "player " ++ name ++ " not in lobby") }
    toServantErr (UnknownTeam team) = err404 { errBody = (pack $ "team " ++ show team ++ " does not exist") }
    toServantErr TeamFull = err409 { errBody = "team already full" }

