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
import Error
import Classes
import Lobby
import Game                         (Game)
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
import Data.ULID

import qualified Data.Map.Strict as Map

type CodenameAPI = LobbyAPI 
              :<|> "lobbies" :> LobbyId :> "events" :> Raw

data Config = Config
    { lobbyState     :: TVar (Map.Map Id Lobby)
    , channels       :: TVar (Map.Map Id (Chan ServerEvent))
    }

type AppM = ReaderT Config (ExceptT ServantErr IO)

type TAPI = "lobbies" :> LobbyId :> (
            Get '[JSON] Lobby
       :<|> "player" :> ReqBody '[JSON] Name :> Post '[JSON] Token)

f :: ((Lobby, [LobbyEvent]) -> a) -> LobbyUpdate -> AppM a
f _ (Left x) = throwError err409 -- TODO add message, somehow decide which status code
f g (Right x) = return $ g x

lobbyServer :: ServerT LobbyAPI AppM
lobbyServer = getLobbies
         :<|> createLobby 
         :<|> withLobby
    where withLobby id = getLobby id
                    :<|> (\name -> modifyLobby id (playerJoin name) (const "token"))
                    :<|> withPlayer id
          withPlayer id token = (\name -> modifyLobby id (playerLeave name) emptyReply)
                           :<|> (\team name -> modifyLobby id (assignTeam team name) emptyReply)
                           :<|> (\team name -> modifyLobby id (unassignTeam name) emptyReply)
                           :<|> (\team -> modifyLobby id (switchRoles team) emptyReply)
                           -- :<|> undefined
          emptyReply = const NoContent

modifyLobby :: Id -> (Lobby -> LobbyUpdate) -> (Lobby -> a) -> AppM a
modifyLobby id updateF returnF = do
    Config {..} <- ask
    result <- liftIO $ atomically $ do 
        lobby <- readTVar lobbyState >>= return . (Map.lookup id)
        case updateF <$> lobby of
            (Just (Right (lobby',events))) -> do 
                modifyTVar' lobbyState $ Map.insert id lobby'
                channel <- readTVar channels >>= return . Map.lookup id
                return $ case channel of
                    Nothing -> Left $ err500 { errBody = "channel missing" }
                    (Just channel) -> Right (lobby',events,channel)
            Nothing  -> return $ Left $ err404 { errBody = "unknown lobby" }
            (Just (Left x)) -> return $ Left $ toServantErr x
    case result of
        (Left x) -> throwError x
        (Right (lobby,events,channel)) -> do
            liftIO $ mapM_ (writeChan channel . serverEvent LobbyChannel) events
            return $ returnF lobby

lobby :: Id -> AppM Lobby
lobby id = do 
    Config { lobbyState = state } <- ask
    lobby <- liftIO $ atomically $ do 
        lobby <- readTVar state >>= return . (Map.lookup id)
        return lobby 
    maybe (throwError err404) return lobby

readerServer :: Config -> Server CodenameAPI
readerServer cfg@(Config {channels = channels}) = enter (readerToExcept cfg) lobbyServer 
    :<|> Tagged . sse channels

-- Type of Application: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
sse :: TVar (Map.Map Id (Chan ServerEvent)) -> Id -> Application
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

createLobby :: GameConfig -> AppM (Id,Lobby)
createLobby (GameConfig {..}) = do
    Config { lobbyState = state, channels = channels } <- ask
    id <- show <$> liftIO getULID
    let lobby = Lobby [] $ Map.fromList $ [(team, Members Nothing Nothing) | team <- [1..numberOfTeams]]
    chan <- liftIO newChan
    liftIO $ atomically $ do
        modifyTVar' state $ Map.insert id lobby
        modifyTVar' channels $ Map.insert id chan
    return (id,lobby)

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
            liftIO $ writeChan chan $ serverEvent LobbyChannel $ PlayerJoined name
            return $ name ++ "-token"
        Nothing -> throwError err404
    where addPlayer (Lobby {..}) = Lobby (name : unassignedPlayers) assignedPlayers
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
