module Trialchain.Transaction where

import Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import Trialchain.Utils

data Payload =
  Payload { from :: Hash
          , to :: Hash
          , amount :: Integer
          }
  deriving (Eq, Show)

instance A.ToJSON Payload where
  toJSON Payload{..} = object [ "from" .= from
                              , "to" .= to
                              , "amount" .= amount
                              ]

instance A.FromJSON Payload where
  parseJSON = withObject "Payload" $
              \ o -> Payload <$> o .: "from" <*> o .: "to" <*> o .: "amount"

instance Hashable Payload where
  hashOf payload = hash $ toStrict $ encode payload

data Transaction =
  Transaction { payload :: Payload
              , previous :: Hash
              , signed :: Signature
              }
  deriving (Eq, Show)

instance A.ToJSON Transaction where
  toJSON Transaction{..} = object [ "payload" .= payload
                                  , "previous" .= previous
                                  , "signed" .= signed
                                  ]

instance A.FromJSON Transaction where
  parseJSON = withObject "Transaction" $
              \ o -> Transaction <$>
                     o .: "payload" <*>
                     o .: "previous" <*>
                     o .: "signed"


signTransaction :: Transaction -> Transaction
signTransaction t = t

-- | The root transaction's hash
baseTransactionHash :: Hash
baseTransactionHash =
  "0000000000000000000000000000000000000000"
