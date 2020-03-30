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


signTransaction :: PrivateKey -> PublicKey -> Transaction -> Transaction
signTransaction privateKey publicKey tx@Transaction{..} =
  tx { signed = signWith privateKey publicKey (hashOf payload <> previous) }

-- | A "seed" `Transaction` that transfers 1_000_000_000 units to given account
-- This function simulates initial injection of "money" into the chain which is
-- usually done through mining or ICOs. It assumes the server is identified
-- with trusted key pair that gives it the power to "make money".
seedTransaction :: PrivateKey -> PublicKey -> Hash -> Transaction
seedTransaction priv pub h =
  signTransaction priv pub tx
  where
    tx = Transaction { payload = Payload { from = baseTransactionHash
                                         , to = h
                                         , amount = 1000000000
                                         }
                     , previous = baseTransactionHash
                     , signed = NotSigned
                     }

isSeedTransaction :: Transaction -> Bool
isSeedTransaction Transaction{payload} = from payload == baseTransactionHash

-- | The root transaction's hash
baseTransactionHash :: Hash
baseTransactionHash =
  "0000000000000000000000000000000000000000"
