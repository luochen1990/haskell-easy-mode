module EasyMode.Crypt (
    module EasyMode.Crypt,
) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import qualified Data.ByteArray
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteArray.Sized (fromByteArrayAccess, unsafeFromByteArrayAccess)
import Data.ByteString.Builder (toLazyByteString)
import EasyMode.Layers.L2

sha256text :: Text -> Text
sha256text = encodeUtf8 >>> hashWith SHA256 >>> convertToBase Base16 >>> decodeUtf8

sha256bytes :: ByteString -> ByteString
sha256bytes = hashWith SHA256 >>> Data.ByteArray.convert
