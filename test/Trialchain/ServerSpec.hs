module Trialchain.ServerSpec where

import Control.Exception (bracket)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.Client
import Servant.Server
import Test.Hspec
import Trialchain.Application
import Trialchain.Server

startServer = startAppServer 0

trialchainServer = bracket startServer stopServer

(registerTrainee :<|> _ )= client registrationAPI

spec :: Spec
spec =
  around trialchainServer $
  describe "Trainees registration" $
  it "answers to trainee registration query" $ \ AppServer{serverPort} -> do
     let trainee = Trainee {name = "Bob", company = "ACME Inc.", address = "10, Main street, 12323 Smalltown"}

     env <- ClientEnv <$> newManager defaultManagerSettings <*> pure (BaseUrl Http "localhost" serverPort "") <*> pure Nothing

     result <- runClientM (registerTrainee "123123" trainee) env

     getResponse <$> result `shouldBe` Right NoContent
