module Trialchain.TransactionSpec where

import Test.Hspec as H
import Test.Hspec.Wai

import Trialchain.Builder

spec :: Spec
spec =
  with mkTrialchainApp $
  describe "Transactions Handling" $ do

    it "on POST /transactions return '201 Created' given Transaction is valid" $ do
      postJSON "/transactions" aValidTransaction `shouldRespondWith` 201

    it "on POST /transactions return '400' given transaction is unsigned" $ do
      postJSON "/transactions" (unsigned aValidTransaction) `shouldRespondWith` 400
