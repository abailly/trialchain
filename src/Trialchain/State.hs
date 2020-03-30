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
import Data.Maybe (isJust)
import Data.Text (Text)
import Trialchain.Identity
import Trialchain.Transaction
import Trialchain.Utils

data Account = Account { identity :: Identity }

data Chain = Chain { serverPrivateKey :: PrivateKey
                   , serverPublicKey :: PublicKey
                   , accounts :: Map.Map Hash Account
                   , transactions :: Map.Map Hash Transaction
                   }

data Event = IdentityRegistered { link :: Text }
           | DuplicateIdentity
           | TransactionRegistered { link :: Text }
           | TransactionUnsigned
           | InvalidSignature
           | UnknownIdentity { idenHash :: Hash }
           | InvalidPreviousTransaction { txHash :: Hash }

type ChainState = TVar Chain

withState ::
  (MonadIO m) => ChainState -> State Chain a -> m a
withState st =
  liftIO . atomically . stateTVar st . runState

initialChain :: PrivateKey -> PublicKey -> Chain
initialChain priv pub = Chain priv pub mempty mempty

initialState :: PrivateKey -> PublicKey -> IO ChainState
initialState priv pub = newTVarIO $ initialChain priv pub

-- * Pure Chain Domain functions

-- | Register a new `Identity`'s `Account` in current `Chain` state
registerIdentity :: Identity -> State Chain Event
registerIdentity identity@Identity{..} = do
  chain@Chain{..} <- get
  case Map.lookup identityId accounts of
    Nothing -> do
      put (chain { accounts = Map.insert identityId (Account identity) accounts })
      void $ registerTransaction (seedTransaction serverPrivateKey serverPublicKey identityId)
      pure (IdentityRegistered $ identityHash identity)
    Just _ -> pure DuplicateIdentity

-- | List all identities known by this `Chain` server
listIdentities :: State Chain [Identity]
listIdentities = gets (fmap identity . Map.elems . accounts)

findAccount :: Hash -> State Chain (Maybe Account)
findAccount h = Map.lookup h <$> gets accounts

-- | Try to register a `Transaction`
-- This function checks the transaction is valid w.r.t. to current state of the ledger
registerTransaction :: Transaction -> State Chain Event
registerTransaction Transaction{signed = NotSigned }  = pure TransactionUnsigned
registerTransaction tx | isSeedTransaction tx =
  gets serverPublicKey >>= flip validateTransactionFrom tx
registerTransaction tx@Transaction{payload} = do
  account <- findAccount (from payload)
  case account of
    Nothing -> pure $ UnknownIdentity (from payload)
    Just (Account Identity{key} ) -> validateTransactionFrom key tx

validateTransactionFrom ::
  PublicKey -> Transaction -> State Chain Event
validateTransactionFrom key tx@Transaction{payload, previous, signed} = do
  let txHash = hashOf payload
  hasPrevious <- transactionExists previous
  if verifySignature key (txHash <> previous) signed
    then if hasPrevious
         then insertTx txHash >> pure (TransactionRegistered $ toText $ txHash)
         else pure $ InvalidPreviousTransaction previous
    else pure $ InvalidSignature
  where
    insertTx h = modify' $
                 \ chain -> chain { transactions =  Map.insert h tx (transactions chain) }

transactionExists :: Hash -> State Chain Bool
transactionExists h | h == baseTransactionHash = pure True
                    | otherwise = isJust <$> getTransaction h

getTransaction :: Hash -> State Chain (Maybe Transaction)
getTransaction h = Map.lookup h <$> gets transactions

listTransactions :: State Chain [Transaction]
listTransactions = Map.elems <$> gets transactions
