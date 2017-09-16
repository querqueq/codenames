module Main where

import Server
import Network.Wai.Logger
import Network.Wai.Handler.Warp     
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TVar  (newTVar)
import qualified Data.Map.Strict as Map

main :: IO ()
main = withStdoutLogger $ \applogger -> do
    let settings = setPort 8080 $ setLogger applogger defaultSettings
    lobbies <- atomically $ newTVar Map.empty
    lobbyChannel <- atomically $ newTVar Map.empty
    let cfg = Config lobbies lobbyChannel
    runSettings settings $ app cfg
