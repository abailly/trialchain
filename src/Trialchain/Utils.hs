module Trialchain.Utils
  ( Hash(..), PublicKey(..), PrivateKey(..)
  , decodeUtf8', toString, hashOf, (</>)
  , generateKeyPair
  ) where

import Control.Monad.Fail (MonadFail)
import Crypto.Error (CryptoFailable(..))
import Crypto.Hash (Digest, SHA1, hash)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Random.Types (MonadRandom)
import qualified Data.Aeson as A
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decode, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)

-- | A SHA1 hash of some value
newtype Hash = Hash { hashValue :: ByteString }
  deriving (Eq, Show, Ord)

instance A.ToJSON Hash where
  toJSON (Hash h) = A.String $ decodeUtf8' h

instance A.FromJSON Hash where
  parseJSON = A.withText "Hash" $ \ s -> pure $ Hash (encodeUtf8 s)


-- | Parse a cryptographic key from a hex-encoded `Text`
parseKey ::
  MonadFail m =>
  (ByteString -> CryptoFailable t) -> (t -> a) -> Text -> m a
parseKey extractor ctor string =
  case (decode $ encodeUtf8 string) of
    (bs, "") -> case extractor bs of
                  CryptoPassed k -> pure $ ctor k
                  CryptoFailed _ -> fail $ "cannot decode key from JSON: " <> show string
    _ -> fail $ "cannot decode key from JSON: " <> show string

-- | A public key
-- The underlying implementation is opaque, here we use Ed25519 elliptic curve-based
-- key pairs
newtype PublicKey = PublicKey { publicKey :: Ed25519.PublicKey }
  deriving (Eq, Show)

instance A.ToJSON PublicKey where
  toJSON (PublicKey key) = A.String $ decodeUtf8' $ encode $ convert key

instance A.FromJSON PublicKey where
  parseJSON = A.withText "PublicKey" $ parseKey Ed25519.publicKey PublicKey

newtype PrivateKey = PrivateKey { privateKey :: Ed25519.SecretKey }
  deriving (Eq, Show)

instance A.ToJSON PrivateKey where
  toJSON (PrivateKey key) = A.String $ decodeUtf8' $ encode $ convert key

instance A.FromJSON PrivateKey where
  parseJSON = A.withText "PrivateKey" $ parseKey Ed25519.secretKey PrivateKey

generateKeyPair :: (MonadRandom m) => m (PublicKey, PrivateKey)
generateKeyPair = do
  secret <- Ed25519.generateSecretKey
  pure (PublicKey (Ed25519.toPublic secret), PrivateKey secret)

-- | Decode a `ByteString` into a `Text` assuming UTF-8 encoding.
-- If unknown bytes sequence are encountered they are replaced by a default
-- character.
decodeUtf8' :: ByteString -> Text
decodeUtf8' = decodeUtf8With lenientDecode

-- | Provide a `String` representation of given JSONable value.
toString :: (A.ToJSON a) => a -> String
toString = unpack . decodeUtf8' . toStrict . A.encode

-- | Returns an hexadecimal encoding of the SHA1 of given bytes
hashOf :: Text -> Hash
hashOf = Hash . convert . h . encodeUtf8
  where
    h :: ByteString -> Digest SHA1
    h = hash

-- | Helper to build URIs
(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
