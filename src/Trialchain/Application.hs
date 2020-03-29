module Trialchain.Application where

import Data.Text (Text)
import Servant
import Trialchain.Identity
import Trialchain.State
import Trialchain.Utils

type API =
  "identities" :> ( ReqBody '[ JSON] Identity :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                    :<|> Get '[ JSON] [Identity]
                  )

api :: Proxy API
api = Proxy

trialchainApp :: ChainState -> Application
trialchainApp state = serve api handlers
  where
    handlers = registerIdentityH :<|> listIdentitiesH

    registerIdentityH identity = do
      result <- withState state (registerIdentity identity)
      case result of
        IdentityRegistered h -> pure $ addHeader ("/identities" </> h) NoContent
        DuplicateIdentity -> throwError err409

    listIdentitiesH = withState state listIdentities
