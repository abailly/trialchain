module Trialchain.Application where

import Data.Text (Text)
import Servant
import Trialchain.Identity
import Trialchain.State
import Trialchain.Transaction
import Trialchain.Utils

type API =
  "identities" :> ( ReqBody '[ JSON] Identity :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                    :<|> Get '[ JSON] [Identity]
                  )
  :<|> "transactions" :>  ReqBody '[ JSON] Transaction :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)

api :: Proxy API
api = Proxy

trialchainApp :: ChainState -> Application
trialchainApp state = serve api handlers
  where
    handlers = (registerIdentityH :<|> listIdentitiesH) :<|> postTransactionH

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
        TransactionRegistered h -> pure $ addHeader ("/identities" </> h) NoContent
        TransactionUnsigned -> throwError err400
        InvalidSignature -> throwError err400
        UnknownIdentity _ -> throwError err400
        _ -> throwError err500
