module Trialchain.RegistrationSpec where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Test.Hspec as H
import Test.Hspec.Wai as W
import Trialchain.Application
import Trialchain.Utils

spec :: Spec
spec =
  with (pure trialchainApp) $
  describe "Trainees registration" $ do

  describe "Given Trainee is not registered" $ do

    it "on POST /<trialchain-id>/trainees return '201 Created'" $ do
      let trainee = Trainee {name = "Bob", company = "ACME Inc.", address = "10, Main street, 12323 Smalltown"}
      postJSON "/1234/trainees" trainee `shouldRespondWith` 201

    it "on POST /<trialchain-id>/trainees return trainee's id as link" $ do
      let trainee = Trainee {name = "Bob", company = "ACME Inc.", address = "10, Main street, 12323 Smalltown"}
      postJSON "/1234/trainees" trainee `shouldRespondWith` ResponseMatcher 201 [("Location" <:> encodeUtf8 ("/1234/trainees/" <> hashOf "Bob"))] ""

  describe "Given Trainee Bob is registered" $ do

    it "on POST /<trialchain-id>/trainees returns 409" $ do
      register bob
      postJSON "/1234/trainees" bob `shouldRespondWith` 409

bob :: Trainee
bob = Trainee {name = "Bob", company = "ACME Inc.", address = "10, Main street, 12323 Smalltown"}

register = postJSON "/1234/trainees"

  -- it "on GET /<trialchain-id>/trainees returns list of registered trainees" $ do
  --   let trainee = Trainee {name = "Bob", company = "ACME Inc.", address = "10, Main street, 12323 Smalltown"}
  --   void $ postJSON "/1234/trainees" trainee
  --   get "/1234/trainees" `shouldRespondWith` (toString [trainee `withI])

postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json")] (A.encode payload)
