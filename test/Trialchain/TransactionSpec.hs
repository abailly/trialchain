module Trialchain.TransactionSpec where

import Data.Functor (void)
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec as H
import Test.Hspec.Wai

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

    it "on GET /transactions/<tx hash> return 200 given tx has been posted" $ do
      register anIdentity
      void $ postJSON "/transactions" aValidTransaction
      let uri = encodeUtf8 $ "/transactions/" <> toText (hashOf $ payload aValidTransaction)

      get uri `shouldRespondWith` 200
