module EasyMode.Cast
( 
    Cast
)
where

import Data.String (String)
import Data.ByteString (ByteString)
import Data.Char (Char)
import Data.Bool (Bool(..), (&&), (||))
import Data.Ord ((<=), (>=), (<), (>))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import GHC.Integer (Integer)
import GHC.Types (Int)
import GHC.Num (Num, (+))
import GHC.Enum (maxBound, minBound)
import GHC.Real (fromIntegral)
import Data.Either.Extra
import Data.List.Extra (chunksOf)
import GHC.Err (error)
import Control.Exception.Extra (Partial)
import Data.List (iterate)
import Data.Foldable (Foldable)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ((&), id, (.))
import Data.List (iterate, unfoldr)
import Data.Text.Encoding (encodeUtf8, decodeUtf8, decodeLatin1)
import Data.Text (Text, pack, unpack)

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
    ocast x = if x <= fromIntegral (maxBound :: Int) && x >= fromIntegral (minBound :: Int) then fromIntegral x else error (unpack "cannot cast Integer to Int since overflow")

