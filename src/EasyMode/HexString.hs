module EasyMode.HexString (
    module Export,
    module EasyMode.HexString,
) where

import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString as Export (ByteString)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Text.Encoding (decodeLatin1)
import qualified Prelude as P

newtype HexString = HexString ByteString

instance P.Show HexString where
    show (HexString bs) = "0x" P.++ unpack (decodeLatin1 (convertToBase Base16 bs))
