module Trialchain.Application where

import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Servant
import Trialchain.Utils

data Account =
  Account { accountId :: Hash
          , key :: PublicKey
          }
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

accountHash :: Account -> Text
accountHash (Account h _) = decodeUtf8' $ hashValue h

type API =
  "accounts" :> ( ReqBody '[ JSON] Account :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
                  :<|> Get '[ JSON] [Account]
                )

api :: Proxy API
api = Proxy

trialchainApp :: Application
trialchainApp = serve api handlers
  where
    handlers = registerAccount :<|> listAccounts
    registerAccount account = pure $ addHeader ("/accounts" </> accountHash account) NoContent
    listAccounts = pure [ ]
