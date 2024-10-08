module EasyMode.Cast (
    module EasyMode.Cast,
) where

import Control.Monad ((>=>))
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import qualified Data.ByteString as BS
import qualified Data.Char as C
import Data.Functor (fmap)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Lazy as HML
import qualified Data.Map.Strict as MS
import qualified Data.Map.Lazy as ML
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text.Lazy as TL
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
class CastTo target source where
    cast :: source -> target

{- |
 minimal impl: ecast | mcast
-}
class PartialCastTo target source where
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

instance CastTo a a where cast = id

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

instance CastTo Text ByteString where cast = decodeUtf8

-- instance Cast Text (Digest a)   where cast = toHex

instance CastTo Text String where cast = pack

instance CastTo Text TL.Text where cast = TL.toStrict

instance CastTo TL.Text Text where cast = TL.fromStrict

instance CastTo TL.Text String where cast = TL.pack

instance CastTo String Text where cast = unpack

instance CastTo String TL.Text where cast = TL.unpack

-- * instances for number

instance CastTo Integer Int where cast = fromIntegral

instance PartialCastTo Int Integer where
    ecast x =
        if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int)
            then Right (fromIntegral x)
            else Left "cannot cast this Integer to Int since accuracy overflow"

instance PartialCastTo Integer String where
    ecast s = case reads s of
        [(x, [])] -> Right x
        [(x, r)] -> Left ("cannot parse this String to Integer, invalid chars: `" ++ pack r ++ "`")
        [] -> Left ("cannot parse this String to Integer: `" ++ pack s ++ "`")
        _ -> impossible

instance PartialCastTo Float64 String where
    ecast s = case reads s of
        [(x, [])] -> Right x
        [(x, r)] -> Left ("cannot parse this String to Float64, invalid chars: `" ++ pack r ++ "`")
        [] -> Left ("cannot parse this String to Float64: `" ++ pack s ++ "`")
        _ -> impossible

instance PartialCastTo Int String where
    ecast = (ecast :: String -> Either Text Integer) >=> ecast

instance PartialCastTo Integer Text where mcast s = mcast (unpack s)

instance PartialCastTo Float64 Text where mcast s = mcast (unpack s)

instance CastTo Integer Bool where cast b = if b then 1 else 0

instance CastTo Int Bool where cast b = if b then 1 else 0

instance CastTo Float64 Int where cast = fromIntegral

instance CastTo Float64 Integer where cast = fromIntegral

instance CastTo Float64 Float32 where cast = realToFrac

instance Integral a => CastTo Float64 (Ratio a) where cast = realToFrac

instance CastTo Float32 Int where cast = fromIntegral

instance CastTo Float32 Integer where cast = fromIntegral

instance CastTo Float32 Float64 where cast = realToFrac

instance Integral a => CastTo Float32 (Ratio a) where cast = realToFrac

-- * instances for hex

instance CastTo ByteString HexString where cast (HexString bs) = bs

instance CastTo HexString ByteString where cast = HexString

instance CastTo Text HexString where cast (HexString bs) = decodeLatin1 (convertToBase Base16 bs)

instance PartialCastTo HexString Text where
    ecast txt = either (Left <<< pack) (Right <<< HexString) (decodeHex (encodeUtf8 txt))
      where
        decodeHex :: ByteString -> Either String ByteString
        decodeHex bs = convertFromBase Base16 (if even (BS.length bs) then bs else "0" ++ bs)

instance CastTo String HexString where cast hex = unpack (toText hex)

instance CastTo Integer HexString where
    cast hs = let [(r, m)] = readHex (toString hs) in r -- TODO: optmize

instance PartialCastTo HexString Integer where
    ecast x = if x >= 0 then Right (pcast (pack (showHex x ""))) else Left "negative Integer cannot be cast to HexString" -- TODO: optmize

-- * instances for list

-- TODO: impl

-- * instances for set

-- TODO: impl

-- * instances for maps

instance CastTo [(k, v)] (HMS.HashMap k v) where cast = HMS.toList

instance CastTo [(k, v)] (ML.Map k v) where cast = ML.toList

instance Hashable k => CastTo (HMS.HashMap k v) [(k, v)] where cast = HMS.fromList

instance Ord k => CastTo (ML.Map k v) [(k, v)] where cast = ML.fromList

-- * helper functions for type cast

toInteger :: PartialCastTo Integer a => a -> Integer
toInteger = pcast

toInt :: PartialCastTo Int a => a -> Int
toInt = pcast

toFloat :: PartialCastTo Float64 a => a -> Float64
toFloat = pcast

toText :: CastTo Text a => a -> Text
toText = cast

toString :: CastTo String a => a -> String
toString = cast

toHex :: CastTo HexString a => a -> HexString
toHex = cast

toList :: CastTo [a] b => b -> [a]
toList = cast

toPairs :: CastTo [(k, v)] (f k v) => f k v -> [(k, v)]
toPairs = cast

toHashMap :: CastTo (HMS.HashMap k v) a => a -> HMS.HashMap k v
toHashMap = cast

toMap :: CastTo (ML.Map k v) a => a -> ML.Map k v
toMap = cast

-- * Python-style helper functions for type cast

int :: PartialCastTo Integer a => a -> Integer
int = toInteger

float :: PartialCastTo Float64 a => a -> Float64
float = toFloat

str :: CastTo Text a => a -> Text
str = toText

chr :: PartialCastTo Int a => a -> Char
chr x = C.chr (pcast x)

ord :: CastTo Char a => a -> Int
ord c = C.ord (cast c)

-- * helper functions for type specification

asInteger :: Integer -> Integer
asInteger = id

asInt :: Int -> Int
asInt = id

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

asMap :: HMS.HashMap k v -> HMS.HashMap k v
asMap = id
