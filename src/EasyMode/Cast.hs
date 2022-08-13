{-# language OverloadedStrings #-}
{-# language ConstraintKinds #-}

module EasyMode.Cast
( 
    module EasyMode.Cast
)
where

import EasyMode.Layers.L1
import GHC.Types (Int)
import Data.String (String)
import Data.Maybe (Maybe(..))
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.Char (ord, isDigit)
import GHC.Real (fromIntegral)
import Prelude ((^), zip, sum, all, reverse)


-- * Cast & PartialCast


class Cast target source where
    cast :: source -> target

class PartialCast target source where
    ocast :: Partial => source -> target -- ^ opinionated cast
    ocast x = case mcast x of
        Nothing -> complain ("opinionated cast failed!")
        Just y -> y

    mcast :: source -> Maybe target


-- * instances for text


instance Cast Text Text         where cast = id

instance Cast Text ByteString   where cast = decodeUtf8


-- instance Cast Text (Digest a)   where cast = toHex


instance Cast Text String       where cast = pack

instance Cast String Text       where cast = unpack


-- * instances for number


instance Cast Integer Int       where cast = fromIntegral

instance PartialCast Int Integer where
    mcast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then Just (fromIntegral x) else Nothing
    ocast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then fromIntegral x else complain ("cannot cast Integer to Int since overflow")

instance PartialCast Integer String where
    mcast s = let ds = reverse s in if all isDigit ds then Just (sum [10^i * fromIntegral (ord d - ord '0') | (i, d) <- zip [0..] ds]) else Nothing

instance PartialCast Integer Text where
    mcast s = mcast (unpack s)


-- * instances for maps


instance Cast [(k, v)] (M.HashMap k v) where
    cast = M.toList

instance Hashable k => Cast (M.HashMap k v) [(k, v)] where
    cast = M.fromList


-- * helper functions


toInteger :: Cast Integer a => a -> Integer
toInteger = cast

toText :: Cast Text a => a -> Text
toText = cast

toPairs :: Cast [(k, v)] (f k v) => f k v -> [(k, v)]
toPairs = cast

toMap :: Hashable k => [(k, v)] -> M.HashMap k v
toMap = cast

