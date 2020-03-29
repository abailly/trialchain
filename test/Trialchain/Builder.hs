{-| Various builders to build test data -}
module Trialchain.Builder where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import System.IO.Unsafe
import Test.Hspec.Wai as W
import Trialchain.Application
import Trialchain.Identity
import Trialchain.State
import Trialchain.Transaction
import Trialchain.Utils


aSecretKey :: (PublicKey, PrivateKey)
aSecretKey = unsafePerformIO generateKeyPair
{-# NOINLINE aSecretKey #-}

anIdentity :: Identity
anIdentity = Identity { identityId = hashOf "alice", key = fst aSecretKey }

alice :: Identity
alice = anIdentity

bobsKeys :: (PublicKey, PrivateKey)
bobsKeys = unsafePerformIO generateKeyPair
{-# NOINLINE bobsKeys #-}

bob :: Identity
bob = Identity { identityId = hashOf "bob", key = fst bobsKeys }

aValidTransaction :: Transaction
aValidTransaction =
  signTransaction tx
  where
    tx = Transaction { payload = payload
                     , previous = baseTransactionHash
                     , signed = NotSigned }
    payload = Payload { from = hashOf "alice"
                      , to = hashOf "bob"
                      , amount = 1
                      }

mkTrialchainApp :: IO Application
mkTrialchainApp = initialState >>= pure . trialchainApp

postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json")] (A.encode payload)
