module Trialchain.ServerSpec where

import Control.Exception (bracket)
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Test.Hspec

import Trialchain.Application
import Trialchain.Builder
import Trialchain.Identity
import Trialchain.Server

startServer :: IO AppServer
startServer = startAppServer 0

trialchainServer :: (AppServer -> IO c) -> IO c
trialchainServer = bracket startServer stopServer

registerIdentity ::
  Identity
  -> ClientM (Headers '[Header "Location" Text] NoContent)
(registerIdentity :<|> _) :<|> (_  :<|> _) = client api

spec :: Spec
spec =
  around trialchainServer $
  describe "Identities API" $
  it "answers to identities query" $ \ AppServer{serverPort} -> do
     env <- ClientEnv <$> newManager defaultManagerSettings <*> pure (BaseUrl Http "localhost" serverPort "") <*> pure Nothing

     result <- runClientM (registerIdentity anIdentity) env

     getResponse <$> result `shouldBe` Right NoContent
