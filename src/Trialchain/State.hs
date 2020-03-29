{-| In-memory state of the Trialchain server

The in-memory state is the result of `apply`ing all transactions from the
ledger /after/ consensus, eg. the state is only "persisted" when new valid
transaction are broadcasted
-}
module Trialchain.State where

import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import qualified Data.Map as Map
import Data.Text (Text)
import Trialchain.Identity
import Trialchain.Utils

data Account = Account { identity :: Identity }

data Chain = Chain { accounts :: Map.Map Hash Account }

data Event = IdentityRegistered { link :: Text }
           | DuplicateIdentity

type ChainState = TVar Chain

withState ::
  (MonadIO m) => ChainState -> State Chain a -> m a
withState st =
  liftIO . atomically . stateTVar st . runState

initialChain :: Chain
initialChain = Chain mempty

initialState :: IO ChainState
initialState = newTVarIO initialChain

-- * Pure Chain Domain functions

-- | Register a new `Identity`'s `Account` in current `Chain` state
registerIdentity :: Identity -> State Chain Event
registerIdentity identity@Identity{..} = do
  chain@Chain{..} <- get
  case Map.lookup identityId accounts of
    Nothing -> put (chain { accounts = Map.insert identityId (Account identity) accounts }) >>
               pure (IdentityRegistered $ identityHash identity)
    Just _ -> pure DuplicateIdentity

-- | List all identities known by this `Chain` server
listIdentities :: State Chain [Identity]
listIdentities = gets (fmap identity . Map.elems . accounts)
