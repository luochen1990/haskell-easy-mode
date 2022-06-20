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
import Data.Maybe (Maybe(..))
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)

-- * Cast & OpinionatedCast

class Cast target source where
    cast :: source -> target

class OpinionatedCast target source where
    ocast :: Partial => source -> target
    ocast x = case mcast x of
        Nothing -> blame ("ocast failed")
        Just y -> y

    mcast :: source -> Maybe target

--class UnsafeCast target source where
--    unsafeCast :: source -> target

--instance Cast target source => OpinionatedCast target source where
--    opinionatedCast = cast

-- * instances

instance Cast Text Text         where cast = id
instance Cast Text ByteString   where cast = decodeUtf8
-- instance Cast Text (Digest a)   where cast = toHex
instance Cast Text String       where cast = pack

instance Cast String Text       where cast = unpack

instance Cast Integer Int       where cast = fromIntegral
instance OpinionatedCast Int Integer where
    mcast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then Just (fromIntegral x) else Nothing

    ocast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then fromIntegral x else blame ("cannot cast Integer to Int since overflow")

instance Cast [(k, v)] (M.HashMap k v) where
    cast = M.toList

instance Hashable k => Cast (M.HashMap k v) [(k, v)] where
    cast = M.fromList

-- * special cast

toText :: Cast Text a => a -> Text
toText = cast

toPairs :: Cast [(k, v)] (f k v) => f k v -> [(k, v)]
toPairs = cast

toMap :: Hashable k => [(k, v)] -> M.HashMap k v
toMap = cast

