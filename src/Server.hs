{-# LANGUAGE DataKinds #-}      
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Server where

import API hiding                   (CodenameAPI)
import Models
import GameModels                   (Game)
import Servant
import Servant.Server
import Network.Wai.Handler.Warp     (run)
import Network.Wai.EventSource
import Control.Concurrent.Chan
import Control.Monad.Reader         --(ReaderT, runReaderT, lift, MonadReader, liftIO)
import Control.Monad.Trans.Except   (ExceptT)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TVar  --(TVar)
import Data.Aeson
import Data.Binary.Builder          (fromLazyByteString)

import qualified Data.Map.Strict as Map

type CodenameAPI = LobbyAPI 
              :<|> "lobbies" :> LobbyId :> "events" :> Raw

data Config = Config
    { lobbyState     :: TVar (Map.Map Id Lobby)
    , channels       :: TVar (Map.Map Id (Chan ServerEvent))
    }

type AppM = ReaderT Config (ExceptT ServantErr IO)

lobbyServer :: ServerT LobbyAPI AppM
lobbyServer = getLobbies
     :<|> createLobby
     :<|> getLobby
     :<|> joinLobby
     :<|> leaveLobby
     :<|> joinTeam
     :<|> leaveTeam
     :<|> switchRole

readerServer :: Config -> Server CodenameAPI
readerServer cfg@(Config {channels = channels}) = enter (readerToExcept cfg) lobbyServer 
    :<|> Tagged . sse channels

-- Type of Application: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
sse :: TVar (Map.Map Id (Chan ServerEvent)) -> Int -> Application
sse channels id  req resp = do
    chan <- liftIO $ atomically $ do
        chans <- readTVar channels
        return $ Map.lookup id chans
    case chan of
        (Just chan) -> eventSourceAppChan chan req resp
        -- FIXME Nothing return 404

readerToExcept :: Config -> AppM :~> Handler
readerToExcept cfg = NT $ \x -> Handler $ runReaderT x cfg

getLobbies :: AppM [(Id,Lobby)]
getLobbies = do
    Config { lobbyState = state } <- ask
    liftIO $ atomically $ do
        lobbies <- readTVar state
        return $ Map.toList lobbies

createLobby :: GameConfig -> AppM Lobby
createLobby (GameConfig {..}) = do
    Config { lobbyState = state, channels = channels } <- ask
    let id = 12
    let lobby = Lobby [] $ Map.fromList $ [(team, Members Nothing Nothing) | team <- [1..numberOfTeams]]
    chan <- liftIO newChan
    liftIO $ atomically $ do
        modifyTVar' state $ Map.insert id lobby
        modifyTVar' channels $ Map.insert id chan
    return lobby

getLobby :: Id -> AppM Lobby
getLobby lid = do
    Config { lobbyState = state } <- ask
    lobby <- liftIO $ atomically $ do
        lobbies <- readTVar state
        return $ Map.lookup lid lobbies
    maybe (throwError err404) return lobby

joinLobby :: Id -> Name -> AppM Token
-- TODO move token to session bzw. make token to session (? still REST?)
joinLobby lid name = do
    Config { lobbyState = state, channels = channels } <- ask
    chan <- liftIO $ atomically $ do
        modifyTVar' state $ Map.adjust addPlayer lid
        chans <- readTVar channels
        return $ Map.lookup lid chans
    case chan of
        (Just chan) -> do
            liftIO $ writeChan chan $ serverEvent LobbyChannel $ Joined name
            return $ name ++ "-token"
        Nothing -> throwError err404
    where addPlayer (Lobby {..}) = Lobby (name : unassignedPlayer) assignedPlayers
leaveLobby = undefined
joinTeam = undefined
leaveTeam = undefined
switchRole = undefined

data Channel = GameChannel | LobbyChannel

channelName GameChannel = "game"
channelName LobbyChannel = "lobby"

serverEvent :: (ToJSON a) => Channel -> a -> ServerEvent
serverEvent channel content = ServerEvent 
    (Just $ fromLazyByteString $ channelName channel)
    Nothing
    [fromLazyByteString $ encode content]

lobbyEvents :: Id -> AppM Application
lobbyEvents lid = do
    Config { channels = channels } <- ask
    chan <- liftIO $ atomically $ do
        chans <- readTVar channels
        return $ Map.lookup lid chans
    maybe (throwError err404) (return . eventSourceAppChan) chan

lobbyEvents' :: Application
lobbyEvents' r = undefined

app :: Config -> Application
app = serve codenameAPI . readerServer

codenameAPI :: Proxy CodenameAPI
codenameAPI = Proxy

test :: IO ()
test = do
    lobbies <- atomically $ newTVar Map.empty
    lobbyChannel <- atomically $ newTVar Map.empty
    let cfg = Config lobbies lobbyChannel
    run 8080 $ app cfg
