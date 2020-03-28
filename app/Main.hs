module Main where

import Control.Concurrent.Async
import Trialchain.Server

main :: IO ()
main = do
  AppServer{serverThread} <- startAppServer 8899
  case serverThread of
    Nothing -> error "Could not start server on port 8899, port might be in use or something bad happened"
    Just th -> wait th
