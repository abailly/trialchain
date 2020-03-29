{-| Various builders to build test data -}
module Trialchain.Builder where

import System.IO.Unsafe
import Trialchain.Identity
import Trialchain.Utils


aSecretKey :: (PublicKey, PrivateKey)
aSecretKey = unsafePerformIO generateKeyPair
{-# NOINLINE aSecretKey #-}

anIdentity :: Identity
anIdentity = Identity { identityId = hashOf "alice", key = fst aSecretKey }
