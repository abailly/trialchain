module Trialchain.Application where

import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Servant
import Trialchain.Utils

data Trainee = Trainee
    { name    :: Text
    , company :: Text
    , address :: Text
    }
    deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

data RegistrationResult = RegistrationResult
    { result :: Text
    }
    deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

type RegistrationAPI
   =    Capture "trialchain-id" Text :> "trainees" :> ReqBody '[ JSON] Trainee :> PostCreated '[ JSON] (Headers '[Header "Location" Text] NoContent)
   :<|> Capture "trialchain-id" Text :> "trainees" :> Get '[ JSON] [Trainee]

registrationAPI :: Proxy RegistrationAPI
registrationAPI = Proxy

trialchainApp :: Application
trialchainApp = serve registrationAPI handlers
  where
    handlers = registerTrainee :<|> listTrainees
    registerTrainee trialchainId Trainee{name} = pure $ addHeader ("/" <> trialchainId </> "trainees" </> hashOf name) NoContent
    listTrainees _ = pure [ Trainee {name = "Alice", company = "ACME Inc.", address = "10, Main street, 12323 Smalltown"}
                          , Trainee {name = "Bob", company = "ACME Inc.", address = "Nantes"}
                          , Trainee {name = "Charlie", company = "Foo SARL.", address = "Bréal-sous-monfort"}
                          ]
