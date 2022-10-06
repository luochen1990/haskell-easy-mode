module EasyMode.HexString (
    module Export,
    module EasyMode.HexString,
) where

import Data.ByteString as Export (ByteString)
import Data.ByteString.Base16 as Base16
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.Text.Encoding (decodeLatin1)
import qualified Prelude as P

newtype HexString = HexString ByteString

instance P.Show HexString where
    show (HexString bs) = "0x" P.++ unpack (decodeLatin1 (Base16.encode bs))
