module Trialchain.IdentitySpec where

import qualified Data.Aeson as A
import Data.Functor (void)
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec as H
import Test.Hspec.Wai as W
import Test.Hspec.Wai.Matcher as W

import Trialchain.Builder
import Trialchain.Identity

spec :: Spec
spec =
  with mkTrialchainApp $
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

    it "on GET /identities returns list of identities" $ do
      register anIdentity
      get "/identities" `shouldRespondWith` ResponseMatcher 200 [] (W.bodyEquals $ A.encode [anIdentity])


register :: Identity -> WaiSession ()
register = void <$> postJSON "/identities"
