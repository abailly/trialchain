module Trialchain.ServerSpec where

import Control.Exception (bracket)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Servant.Server
import Test.Hspec
import Trialchain.Application
import Trialchain.Builder
import Trialchain.Server

startServer = startAppServer 0

trialchainServer = bracket startServer stopServer

(registerAccount :<|> _ )= client api

spec :: Spec
spec =
  around trialchainServer $
  describe "Trainees registration" $
  it "answers to trainee registration query" $ \ AppServer{serverPort} -> do

     env <- ClientEnv <$> newManager defaultManagerSettings <*> pure (BaseUrl Http "localhost" serverPort "") <*> pure Nothing

     result <- runClientM (registerAccount anAccount) env

     getResponse <$> result `shouldBe` Right NoContent
