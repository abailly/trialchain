module Trialchain.Account where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import GHC.Natural
import Trialchain.Identity

data Account = Account { identity :: Identity
                       , balance :: Natural
                       }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
