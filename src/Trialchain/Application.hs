module Trialchain.Application where

import Data.Text (Text)
import Servant
import Trialchain.Identity
import Trialchain.Utils

type API =
  "identities" :> ( ReqBody '[ JSON] Identity :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                    :<|> Get '[ JSON] [Identity]
                  )

api :: Proxy API
api = Proxy

trialchainApp :: Application
trialchainApp = serve api handlers
  where
    handlers = registerIdentity :<|> listIdentities
    registerIdentity identity = pure $ addHeader ("/identities" </> identityHash identity) NoContent
    listIdentities = pure [ ]
