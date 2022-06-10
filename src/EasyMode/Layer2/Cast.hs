{-# language OverloadedStrings #-}
{-# language ConstraintKinds #-}

module EasyMode.Layer2.Cast
( 
    module EasyMode.Layer2.Cast
)
where

import EasyMode.Layer1
import GHC.Types (Int)
import Data.String (String)

-- * Cast & OpinionatedCast

class Cast target source where
    cast :: source -> target

class OpinionatedCast target source where
    ocast :: Partial => source -> target

--class UnsafeCast target source where
--    unsafeCast :: source -> target

--instance Cast target source => OpinionatedCast target source where
--    opinionatedCast = cast

-- * ToText

type ToText = Cast Text

-- * instances

instance Cast Text Text         where cast = id
instance Cast Text ByteString   where cast = decodeUtf8
-- instance Cast Text (Digest a)   where cast = toHex
instance Cast Text String       where cast = pack

instance Cast String Text       where cast = unpack

instance Cast Integer Int       where cast = fromIntegral
instance OpinionatedCast Int Integer where
    ocast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then fromIntegral x else blame ("cannot cast Integer to Int since overflow")

