module Trialchain.Server where

import Control.Concurrent.Async
import Data.Default
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Trialchain.Application
import Trialchain.State

data AppServer = AppServer
    { serverThread :: Maybe (Async ())
    , serverPort   :: Port
    }

startAppServer :: Port -> IO AppServer
startAppServer listenPort = do
  logger             <- doLog
  _state              <- initialState
  (realPort, thread) <- server logger
  pure $ AppServer (Just thread) realPort
    where
      makeWarpRunner =
        if listenPort /= 0
        then pure (listenPort, Warp.run listenPort)
        else openFreePort >>= \ (port, socket) -> pure (port, Warp.runSettingsSocket defaultSettings socket)

      server logger = do
        (realPort, appRunner) <- makeWarpRunner
        thread <- async $ appRunner $ logger $ trialchainApp
        pure (realPort, thread)

      doLog = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

stopServer :: AppServer -> IO ()
stopServer (AppServer (Just thread) _) = cancel thread
stopServer _                           = pure ()
