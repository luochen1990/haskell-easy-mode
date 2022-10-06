module EasyMode.HexString (
    module Export,
    module EasyMode.HexString,
) where

import Data.ByteString as Export (ByteString)

newtype HexString = HexString ByteString
