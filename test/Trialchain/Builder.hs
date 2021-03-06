{-| Various builders to build test data -}
module Trialchain.Builder where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Functor (void)
import Data.Text (Text)
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
anIdentity = Identity { identityId = hashOf @Text  "alice", key = fst aSecretKey }

alice :: Identity
alice = anIdentity

bobsKeys :: (PublicKey, PrivateKey)
bobsKeys = unsafePerformIO generateKeyPair
{-# NOINLINE bobsKeys #-}

bob :: Identity
bob = Identity { identityId = hashOf @Text "bob", key = fst bobsKeys }

aValidTransaction :: Transaction
aValidTransaction =
  signTransaction priv pub tx
  where
    (pub, priv) = aSecretKey
    tx = Transaction { payload = payload
                     , previous = baseTransactionHash
                     , signed = NotSigned }
    payload = Payload { from = hashOf @Text "alice"
                      , to = hashOf @Text "bob"
                      , amount = 1
                      }

unsigned :: Transaction -> Transaction
unsigned tx = tx { signed = NotSigned }

serverKeys :: (PublicKey, PrivateKey)
serverKeys = unsafePerformIO generateKeyPair
{-# NOINLINE serverKeys #-}

mkTrialchainApp :: IO Application
mkTrialchainApp = initialState priv pub  >>= pure . trialchainApp
  where
    (pub, priv) = serverKeys


postJSON :: (A.ToJSON a) => ByteString -> a -> WaiSession SResponse
postJSON path payload = request "POST" path [("Content-type", "application/json")] (A.encode payload)

register :: Identity -> WaiSession ()
register = void . postJSON "/identities"
