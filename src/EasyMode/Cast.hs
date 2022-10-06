{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module EasyMode.Cast (
    module EasyMode.Cast,
) where

import Control.Monad ((>=>))
import Data.Char (isDigit, ord)
import Data.Functor (fmap)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.List (head, length, (!!))
import Data.Maybe (Maybe (..))
import Data.String (String)
import Data.Tuple (fst, snd)
import EasyMode.Layers.L1
import GHC.Real (fromIntegral)
import GHC.Types (Int)
import Numeric (readHex)
import Numeric.Extra (showHex)
import Text.Hex (decodeHex, encodeHex)
import Prelude (Rational, all, const, either, maybe, realToFrac, reverse, sum, zip, (^))

-- * Cast & PartialCast

{- |
 Rule 1: cast should not lost information:

    forall a b: Type, x: a.  (pcast (cast x :: b) :: a) == x
-}
class Cast target source where
    cast :: source -> target

{- |
 minimal impl: ecast | mcast
-}
class PartialCast target source where
    -- | opinionated partial cast
    pcast :: Partial => source -> target
    pcast x = case ecast x of
        Left msg -> complain msg
        Right r -> r

    -- | partial cast which returns Maybe
    mcast :: source -> Maybe target
    mcast x = either (const Nothing) Just (ecast x)

    -- | partial cast which returns (Either Text)
    ecast :: source -> Either Text target
    ecast x = maybe (Left "partial cast failed!") Right (mcast x)

-- * reflexive instance

instance Cast a a where cast = id

-- * transitive instances

{-
-- These can be Problematic: https://stackoverflow.com/questions/34318707/multiparameter-typeclasses-and-illegal-instance-declarations

instance (Cast c b, Cast b a) => Cast c a where
    cast = cast . cast

instance (PartialCast c b, PartialCast b a) => PartialCast c a where
    mcast = mcast . mcast
    pcast = pcast . pcast
-}

-- * instances for text

instance Cast Text ByteString where cast = decodeUtf8

-- instance Cast Text (Digest a)   where cast = toHex

instance Cast Text String where cast = pack

instance Cast String Text where cast = unpack

-- * instances for number

instance Cast Integer Int where cast = fromIntegral

instance PartialCast Int Integer where
    ecast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then Right (fromIntegral x) else Left "cannot cast this Integer to Int since accuracy overflow"

instance PartialCast Integer String where
    ecast s = let ds = reverse s in if all isDigit ds then Right (sum [10 ^ i * fromIntegral (ord d - ord '0') | (i, d) <- zip [0 ..] ds]) else Left "cannot parse this String to Integer"

instance PartialCast Int String where
    ecast = (ecast :: String -> Either Text Integer) >=> ecast

instance PartialCast Integer Text where mcast s = mcast (unpack s)

instance Cast Integer Bool where cast b = if b then 1 else 0

instance Cast Int Bool where cast b = if b then 1 else 0

instance Cast Float64 Int where cast = fromIntegral

instance Cast Float64 Integer where cast = fromIntegral

instance Cast Float64 Float32 where cast = realToFrac

instance Integral a => Cast Float64 (Ratio a) where cast = realToFrac

instance Cast Float32 Int where cast = fromIntegral

instance Cast Float32 Integer where cast = fromIntegral

instance Cast Float32 Float64 where cast = realToFrac

instance Integral a => Cast Float32 (Ratio a) where cast = realToFrac

-- * instances for hex

instance Cast ByteString HexString where cast (HexString bs) = bs

instance Cast HexString ByteString where cast bs = HexString bs

instance Cast Text HexString where cast (HexString bs) = encodeHex bs

instance PartialCast HexString Text where mcast txt = fmap HexString (decodeHex txt)

instance Cast String HexString where cast hex = unpack (toText hex)

instance Cast Integer HexString where
    cast (HexString bs) = let (r, m) = head (readHex (unpack (toText bs))) in assert (length m == 0) r -- TODO: optmize

instance Cast HexString Integer where
    cast x = pcast (pack (showHex x "")) -- TODO: optmize

-- * instances for maps

instance Cast [(k, v)] (M.HashMap k v) where cast = M.toList

instance Hashable k => Cast (M.HashMap k v) [(k, v)] where cast = M.fromList

-- * helper functions

toInteger :: Cast Integer a => a -> Integer
toInteger = cast

toFloat :: Cast Float64 a => a -> Float64
toFloat = cast

toText :: Cast Text a => a -> Text
toText = cast

toPairs :: Cast [(k, v)] (f k v) => f k v -> [(k, v)]
toPairs = cast

toMap :: Hashable k => [(k, v)] -> M.HashMap k v
toMap = cast
