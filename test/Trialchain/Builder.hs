{-| Various builders to build test data -}
module Trialchain.Builder where

import System.IO.Unsafe
import Trialchain.Application
import Trialchain.Utils


aSecretKey :: (PublicKey, PrivateKey)
aSecretKey = unsafePerformIO generateKeyPair
{-# NOINLINE aSecretKey #-}

anAccount :: Account
anAccount = Account { accountId = hashOf "alice", key = fst aSecretKey }
