module Trialchain.Utils where

import Crypto.Hash (Digest(..), SHA1, hash)
import Data.Aeson as A
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)


-- | Decode a `ByteString` into a `Text` assuming UTF-8 encoding.
-- If unknown bytes sequence are encountered they are replaced by a default
-- character.
decodeUtf8' :: ByteString -> Text
decodeUtf8' = decodeUtf8With lenientDecode

-- | Provide a `String` representation of given JSONable value.
toString :: (A.ToJSON a) => a -> String
toString = unpack . decodeUtf8' . toStrict . A.encode

-- | Returns an hexadecimal encoding of the SHA1 of given bytes
hashOf :: Text -> Text
hashOf = decodeUtf8' . convert . h . encodeUtf8
  where
    h :: ByteString -> Digest SHA1
    h = hash

-- | Helper to build URIs
(</>) :: Text -> Text -> Text
a </> b = a <> "/" <> b
