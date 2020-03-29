{-| In-memory state of the Trialchain server

The in-memory state is the result of `apply`ing all transactions from the
ledger /after/ consensus, eg. the state is only "persisted" when new valid
transaction are broadcasted
-}
module Trialchain.State where

import qualified Data.Map as Map
import Trialchain.Identity
import Trialchain.Utils

data Account = Account { identity :: Identity }

data Chain = Chain { accounts :: Map.Map Hash Account }

initialState :: IO Chain
initialState = undefined
