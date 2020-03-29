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
