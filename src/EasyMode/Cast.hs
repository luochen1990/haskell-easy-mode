module EasyMode.Cast
( 
    Cast
)
where

import EasyMode.Basics

-- * Cast

class Cast b a where
    cast :: a -> b

class OpinionatedCast b a where
    ocast :: Partial => a -> b

--class UnsafeCast b a where
--    unsafeCast :: a -> b

--instance Cast b a => OpinionatedCast b a where
--    opinionatedCast = cast

instance Cast Text Text         where cast = id
instance Cast Text ByteString   where cast = decodeUtf8
-- instance Cast Text (Digest a)   where cast = toHex
instance Cast Text String       where cast = pack

instance Cast String Text       where cast = unpack

instance Cast Integer Int       where cast = fromIntegral
instance OpinionatedCast Int Integer where
    ocast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then fromIntegral x else error ("cannot cast Integer to Int since overflow")

