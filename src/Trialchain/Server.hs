module Trialchain.Server where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Default
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Trialchain.Application
import Trialchain.State

data AppServer = AppServer
    { serverThread :: Maybe (Async ())
    , serverPort   :: Port
    , serverState :: TVar Chain
    }

startAppServer :: Port -> IO AppServer
startAppServer listenPort = do
  logger             <- doLog
  state              <- initialState
  (realPort, thread) <- server logger state
  pure $ AppServer (Just thread) realPort state
    where
      makeWarpRunner =
        if listenPort /= 0
        then pure (listenPort, Warp.run listenPort)
        else openFreePort >>= \ (port, socket) -> pure (port, Warp.runSettingsSocket defaultSettings socket)

      server logger state = do
        (realPort, appRunner) <- makeWarpRunner
        thread <- async $ appRunner $ logger $ trialchainApp state
        pure (realPort, thread)

      doLog = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

stopServer :: AppServer -> IO ()
stopServer (AppServer (Just thread) _ _) = cancel thread
stopServer _                           = pure ()
