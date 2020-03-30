{-| In-memory state of the Trialchain server

The in-memory state is the result of `apply`ing all transactions from the
ledger /after/ consensus, eg. the state is only "persisted" when new valid
transaction are broadcasted
-}
module Trialchain.State where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans (MonadIO(..))
import Data.Aeson (ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Natural
import Trialchain.Account
import Trialchain.Identity
import Trialchain.Transaction
import Trialchain.Utils

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
           | NotEnoughBalance
  deriving (Eq, Show, Generic, ToJSON)

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
      put (chain { accounts = Map.insert identityId (Account identity 0) accounts })
      void $ registerTransaction (seedTransaction serverPrivateKey serverPublicKey identityId)
      pure (IdentityRegistered $ identityHash identity)
    Just _ -> pure DuplicateIdentity

-- | List all identities known by this `Chain` server
listIdentities :: State Chain [Identity]
listIdentities = gets (fmap identity . Map.elems . accounts)

getIdentity :: Hash -> State Chain (Maybe Identity)
getIdentity h = fmap identity . Map.lookup h <$> gets accounts

findAccount :: Hash -> State Chain (Maybe Account)
findAccount h = Map.lookup h <$> gets accounts

listAccounts :: State Chain [Account]
listAccounts = Map.elems <$> gets accounts

getCurrentBalance :: Hash -> State Chain Natural
getCurrentBalance = (maybe 0 balance <$>) . findAccount

checkAccountHasEnoughBalance ::
  Hash -> Natural -> ExceptT Event (State Chain) ()
checkAccountHasEnoughBalance h amount
  | h == baseTransactionHash = pure ()
  | otherwise = do
      bal <- lift (getCurrentBalance h)
      if bal >= amount
        then pure ()
        else throwError NotEnoughBalance

updateBalances :: Payload -> State Chain ()
updateBalances Payload{..} = do
  modify' (updateBalance to (+))
  modify' (updateBalance from (-))
  where
    updateBalance h op chain = chain { accounts = Map.update (updateBal op) h (accounts chain) }
    updateBal op acc = Just $ acc { balance = balance acc `op` amount }

-- | Try to register a `Transaction`
-- This function checks the transaction is valid w.r.t. to current state of the ledger
-- As we don't distinguish errors from `Event`s we return this odd `Either Event Event`
-- type, with the usual semantics that errors are on the `Left` and successes on the
-- `Right`.
registerTransaction :: Transaction -> State Chain (Either Event Event)
registerTransaction Transaction{signed = NotSigned } =
  pure $ Left TransactionUnsigned

registerTransaction tx | isSeedTransaction tx =
  gets serverPublicKey >>= runExceptT . flip validateTransactionFrom tx

registerTransaction tx@Transaction{payload} = runExceptT $ do
  Account{identity = Identity{key}} <- checkAccountExists (from payload)
  void $ checkAccountExists (to payload)
  validateTransactionFrom key tx

-- | Internal validation logic for new `Transaction`s.
--
-- We wrap this computation in an  `ExceptT` monad transformer in order to simplify
-- writing validation logic as a sequence of steps: Each step in the sequence can
-- fail in different ways and we want to shortcut computation when it fails.
validateTransactionFrom ::
  PublicKey -> Transaction -> ExceptT Event (State Chain) Event
validateTransactionFrom key tx@Transaction{payload, previous, signed} = do
  let txHash = hashOf payload
  checkTransactionExists previous
  checkAccountHasEnoughBalance (from payload) (amount payload)
  checkSignature key (txHash <> previous) signed
  lift $ insertTx txHash >> updateBalances payload >> pure (TransactionRegistered $ toText $ txHash)
  where
    insertTx h = modify' $
                 \ chain -> chain { transactions =  Map.insert h tx (transactions chain) }

checkAccountExists ::
  Hash -> ExceptT Event (State Chain) Account
checkAccountExists h = do
  account <- lift $ findAccount h
  maybe (throwError $ UnknownIdentity h) pure account

checkSignature ::
  PublicKey -> Hash -> Signature -> ExceptT Event (State Chain) ()
checkSignature key h signed =
  if verifySignature key h signed
  then pure ()
  else throwError InvalidSignature

checkTransactionExists ::
  Hash -> ExceptT Event (State Chain) ()
checkTransactionExists h | h == baseTransactionHash = pure ()
                    | otherwise = lift (getTransaction h) >>=
                                  maybe (throwError $ InvalidPreviousTransaction h) (const $ pure ())


getTransaction ::
  Hash -> State Chain (Maybe Transaction)
getTransaction h = Map.lookup h <$> gets transactions

listTransactions ::
  State Chain [Transaction]
listTransactions = Map.elems <$> gets transactions
