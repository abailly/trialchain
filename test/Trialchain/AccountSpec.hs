module Trialchain.AccountSpec where

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
import Trialchain.Builder
import Trialchain.Utils

spec :: Spec
spec =
  with (pure trialchainApp) $
  describe "Accounts registration" $ do

  describe "Given Account is not registered" $ do

    it "on POST /accounts return '201 Created'" $ do
      postJSON "/accounts" anAccount `shouldRespondWith` 201

    it "on POST /accounts return trainee's id as link" $ do
      postJSON "/accounts" anAccount `shouldRespondWith` ResponseMatcher 201 [("Location" <:> encodeUtf8 ("/accounts/" <> accountHash anAccount))] ""

  describe "Given Trainee Bob is registered" $ do

    it "on POST /accounts returns 409" $ do
      register anAccount
      postJSON "/accounts" anAccount `shouldRespondWith` 409

register = postJSON "/accounts"

  -- it "on GET /accounts returns list of registered accounts" $ do
  --   register anAccount
  --   get "/accounts" `shouldRespondWith` (toString [anAccount])

postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json")] (A.encode payload)
