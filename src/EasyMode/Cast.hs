{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module EasyMode.Cast (
    module EasyMode.Cast,
) where

import Control.Monad ((>=>))
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import Data.Char (isDigit, ord)
import Data.Functor (fmap)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.List (head, length, (!!))
import Data.Maybe (Maybe (..))
import Data.String (String)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Tuple (fst, snd)
import EasyMode.Layers.L1
import GHC.Real (fromIntegral)
import GHC.Types (Int)
import Numeric (readHex)
import Numeric.Extra (showHex)
import Prelude (Rational, all, const, either, even, maybe, reads, realToFrac, reverse, sum, zip, (^))

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
    ecast x =
        if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int)
            then Right (fromIntegral x)
            else Left "cannot cast this Integer to Int since accuracy overflow"

instance PartialCast Integer String where
    ecast s = case reads s of
        [(x, [])] -> Right x
        [(x, r)] -> Left ("cannot parse this String to Integer, invalid chars: `" ++ pack r ++ "`")
        _ -> impossible

instance PartialCast Float64 String where
    ecast s = case reads s of
        [(x, [])] -> Right x
        [(x, r)] -> Left ("cannot parse this String to Float64, invalid chars: `" ++ pack r ++ "`")
        _ -> impossible

instance PartialCast Int String where
    ecast = (ecast :: String -> Either Text Integer) >=> ecast

instance PartialCast Integer Text where mcast s = mcast (unpack s)

instance PartialCast Float64 Text where mcast s = mcast (unpack s)

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

instance Cast Text HexString where cast (HexString bs) = decodeLatin1 (convertToBase Base16 bs)

instance PartialCast HexString Text where
    ecast txt = either (Left <<< pack) (Right <<< HexString) (decodeHex (encodeUtf8 txt))
      where
        decodeHex :: ByteString -> Either String ByteString
        decodeHex bs = convertFromBase Base16 (if even (BS.length bs) then bs else "0" ++ bs)

instance Cast String HexString where cast hex = unpack (toText hex)

instance Cast Integer HexString where
    cast hs = let [(r, m)] = (readHex (toString hs)) in r -- TODO: optmize

instance PartialCast HexString Integer where
    ecast x = if x >= 0 then Right (pcast (pack (showHex x ""))) else Left "negative Integer cannot be cast to HexString" -- TODO: optmize

-- * instances for maps

instance Cast [(k, v)] (M.HashMap k v) where cast = M.toList

instance Hashable k => Cast (M.HashMap k v) [(k, v)] where cast = M.fromList

-- * helper functions

toInteger :: PartialCast Integer a => a -> Integer
toInteger = pcast

toFloat :: PartialCast Float64 a => a -> Float64
toFloat = pcast

toText :: Cast Text a => a -> Text
toText = cast

toString :: Cast String a => a -> String
toString = cast

toHex :: Cast HexString a => a -> HexString
toHex = cast

toPairs :: Cast [(k, v)] (f k v) => f k v -> [(k, v)]
toPairs = cast

toMap :: Hashable k => [(k, v)] -> M.HashMap k v
toMap = cast

asInteger :: Integer -> Integer
asInteger = id

asFloat :: Float64 -> Float64
asFloat = id

asText :: Text -> Text
asText = id

asString :: String -> String
asString = id

asHex :: HexString -> HexString
asHex = id

asPairs :: [(k, v)] -> [(k, v)]
asPairs = id

asMap :: M.HashMap k v -> M.HashMap k v
asMap = id
