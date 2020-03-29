{-| Represents a unique `Identity` in the trialchain network
By convention, there cannot be two different entities with the same `Hash` value.
-}
module Trialchain.Identity where

import Data.Aeson as A
import Data.Text (Text)
import GHC.Generics
import Trialchain.Utils

data Identity =
  Identity { identityId :: Hash
           , key :: PublicKey
           }
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

identityHash :: Identity -> Text
identityHash (Identity h _) = decodeUtf8' $ hashValue h
