module Trialchain.TransactionSpec where

import Test.Hspec as H
import Test.Hspec.Wai

import Trialchain.Builder
import Trialchain.Transaction

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
