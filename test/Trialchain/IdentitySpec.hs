module Trialchain.IdentitySpec where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai.Test (SResponse)
import Test.Hspec as H
import Test.Hspec.Wai as W

import Trialchain.Application
import Trialchain.Builder
import Trialchain.Identity

spec :: Spec
spec =
  with (pure trialchainApp) $
  describe "Identities registration" $ do

  describe "Given Identity is not registered" $ do

    it "on POST /identities return '201 Created'" $ do
      postJSON "/identities" anIdentity `shouldRespondWith` 201

    it "on POST /identities return trainee's id as link" $ do
      postJSON "/identities" anIdentity `shouldRespondWith` ResponseMatcher 201 [("Location" <:> encodeUtf8 ("/identities/" <> identityHash anIdentity))] ""

  describe "Given Identity is registered" $ do

    it "on POST /identities returns 409" $ do
      register anIdentity
      postJSON "/identities" anIdentity `shouldRespondWith` 409

register :: Identity -> WaiSession ()
register = void <$> postJSON "/identities"

  -- it "on GET /identities returns list of registered identities" $ do
  --   register anIdentity
  --   get "/identities" `shouldRespondWith` (toString [anIdentity])

postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json")] (A.encode payload)
