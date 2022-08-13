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
import Prelude ((^), zip, sum, all, reverse, Double, Float, Rational, realToFrac)


-- * Cast & PartialCast

{- Rule:
    forall a b: Type, x: a.  (ocast (cast x :: b) :: a) == x

 -}

class Cast target source where
    cast :: source -> target

class PartialCast target source where
    ocast :: Partial => source -> target -- ^ opinionated cast
    ocast x = case mcast x of
        Nothing -> complain ("opinionated cast failed!")
        Just y -> y

    mcast :: source -> Maybe target


-- * reflexive instance


instance Cast a a where cast = id


-- * transitive instances


{-
-- These can be Problematic: https://stackoverflow.com/questions/34318707/multiparameter-typeclasses-and-illegal-instance-declarations

instance (Cast c b, Cast b a) => Cast c a where
    cast = cast . cast

instance (PartialCast c b, PartialCast b a) => PartialCast c a where
    mcast = mcast . mcast
    ocast = ocast . ocast
-}


-- * instances for text


instance Cast Text ByteString where cast = decodeUtf8


-- instance Cast Text (Digest a)   where cast = toHex


instance Cast Text String where cast = pack

instance Cast String Text where cast = unpack


-- * instances for number


instance Cast Integer Int where cast = fromIntegral

instance PartialCast Int Integer where
    mcast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then Just (fromIntegral x) else Nothing
    ocast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then fromIntegral x else complain ("cannot cast Integer to Int since overflow")

instance PartialCast Integer String where
    mcast s = let ds = reverse s in if all isDigit ds then Just (sum [10^i * fromIntegral (ord d - ord '0') | (i, d) <- zip [0..] ds]) else Nothing

instance PartialCast Integer Text where mcast s = mcast (unpack s)

instance Cast Integer Bool where cast b = if b then 1 else 0

instance Cast Int Bool where cast b = if b then 1 else 0

instance Cast Double Int where cast = fromIntegral

instance Cast Double Integer where cast = fromIntegral

instance Cast Double Float where cast = realToFrac 

instance Cast Float Int where cast = fromIntegral

instance Cast Float Integer where cast = fromIntegral

instance Cast Float Double where cast = realToFrac 


-- * instances for maps


instance Cast [(k, v)] (M.HashMap k v) where cast = M.toList

instance Hashable k => Cast (M.HashMap k v) [(k, v)] where cast = M.fromList


-- * helper functions


toInteger :: Cast Integer a => a -> Integer
toInteger = cast

toText :: Cast Text a => a -> Text
toText = cast

toPairs :: Cast [(k, v)] (f k v) => f k v -> [(k, v)]
toPairs = cast

toMap :: Hashable k => [(k, v)] -> M.HashMap k v
toMap = cast

