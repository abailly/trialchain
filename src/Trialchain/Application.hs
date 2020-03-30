module Trialchain.Application where

import Data.Aeson (encode)
import Data.Text (Text)
import Servant
import Trialchain.Account
import Trialchain.Identity
import Trialchain.State
import Trialchain.Transaction
import Trialchain.Utils

type API =
  "identities" :> ( ReqBody '[ JSON] Identity :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                    :<|> Get '[ JSON] [Identity]
                  )
  :<|> "transactions" :>  ( ReqBody '[ JSON] Transaction :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                            :<|> Capture "txHash" Hash :> Get '[JSON] Transaction
                            :<|> Get '[JSON] [Transaction]
                          )
  :<|> "accounts" :> Get '[JSON] [Account]

api :: Proxy API
api = Proxy

trialchainApp :: ChainState -> Application
trialchainApp state = serve api handlers
  where
    handlers = (registerIdentityH :<|> listIdentitiesH)
               :<|> (postTransactionH :<|> getTransactionH :<|> listTransactionsH)
               :<|> listAccountsH

    registerIdentityH identity = do
      result <- withState state (registerIdentity identity)
      case result of
        IdentityRegistered h -> pure $ addHeader ("/identities" </> h) NoContent
        DuplicateIdentity -> throwError err409
        _ -> throwError err500

    listIdentitiesH = withState state listIdentities

    postTransactionH tx = do
      result <- withState state (registerTransaction tx)
      case result of
        Right (TransactionRegistered h) -> pure $ addHeader ("/identities" </> h) NoContent
        Left err -> throwError $ err400 { errBody = encode err }
        _ -> throwError err500

    getTransactionH h = do
      maybeTx <- withState state $ getTransaction h
      case maybeTx of
        Nothing -> throwError err404
        Just tx -> pure tx

    listTransactionsH = withState state $ listTransactions

    listAccountsH = withState state $ listAccounts
