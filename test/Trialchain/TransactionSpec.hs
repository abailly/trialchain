module Trialchain.TransactionSpec where

import qualified Data.Aeson as A
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec as H
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher as W

import Trialchain.Builder
import Trialchain.Transaction
import Trialchain.Utils


spec :: Spec
spec =
  with mkTrialchainApp $
  describe "Transactions Handling" $ do

    it "on POST /transactions return '201 Created' given Transaction is valid" $ do
      register anIdentity
      postJSON "/transactions" aValidTransaction `shouldRespondWith` 201

    it "on POST /transactions return '400' given transaction is unsigned" $ do
      register anIdentity
      postJSON "/transactions" (unsigned aValidTransaction) `shouldRespondWith` 400

    it "on POST /transactions return '400' given transaction is not signed by issuer" $ do
      let (pub, priv) = bobsKeys
      register anIdentity
      postJSON "/transactions" (signTransaction priv pub aValidTransaction) `shouldRespondWith` 400

    it "on POST /transactions return '400' given transaction signed by unknown issuer" $ do
      postJSON "/transactions" aValidTransaction `shouldRespondWith` 400

    it "on POST /transactions return '400' given previous transaction does not exist" $ do
      let (pub, priv) = aSecretKey
          wrongPreviousTx = signTransaction priv pub (aValidTransaction { previous = (hashOf @Text "foo") })
      register anIdentity

      postJSON "/transactions" wrongPreviousTx `shouldRespondWith`
        ResponseMatcher 400 [] (W.bodyEquals "Invalid previous transaction")

    it "on GET /transactions/<tx hash> return 200 given tx has been posted" $ do
      register anIdentity
      void $ postJSON "/transactions" aValidTransaction
      let uri = encodeUtf8 $ "/transactions/" <> toText (hashOf $ payload aValidTransaction)

      get uri `shouldRespondWith` 200

    it "on GET /transactions/<tx hash> return 404 given tx hash is not known" $ do
      get "/transactions/0123456789012345678901234567890123456789" `shouldRespondWith` 404

    describe "Accounts Balances" $ do

      it "inserts a transaction for 1000000000 Trials when registering identity" $ do
        let (pub, priv) = serverKeys
        register anIdentity
        get "/transactions" `shouldRespondWith`
          ResponseMatcher 200 [] (W.bodyEquals $ A.encode [seedTransaction priv pub $ hashOf @Text "alice"])
