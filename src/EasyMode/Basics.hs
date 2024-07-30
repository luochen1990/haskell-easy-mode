module EasyMode.Basics (
    module Export,
    module EasyMode.Basics,
) where

-- basic types

import Data.Bool as Export (Bool (..), (&&), (||))
import Data.ByteString as Export (ByteString)
import Data.Char as Export (Char)
import Data.Text as Export (Text, pack, unpack)

-- import Data.String as Export (String)
import Data.Text.Encoding as Export (decodeLatin1, decodeUtf8, encodeUtf8)

-- number types

import Data.Eq as Export (Eq (..))
import Data.Ord as Export (Ord (..))
import GHC.Enum as Export (maxBound, minBound)
import GHC.Float as Export ((**))
import qualified GHC.Float as P (Double, Float)
import GHC.Integer as Export (Integer)
import GHC.Num as Export (Integer (..), Num (..), subtract)
import GHC.Real as Export (Integral, Ratio, Rational, ceiling, div, divMod, floor, mod, properFraction, round, truncate, (/))
import qualified GHC.Real as P ((%))
import qualified Prelude as P (Int)

-- simple container types

import Data.Either as Export (Either (..))
import Data.Either.Extra as Export (fromLeft', fromRight', mapLeft, mapRight)
import Data.Maybe as Export (Maybe (..), fromJust)
import Data.Monoid as Export (Endo (..), Monoid (..))

-- list types

import Data.Foldable as Export (Foldable)
import qualified Data.Foldable as Foldable
import Data.List as Export (iterate, unfoldr)
import qualified Data.List as List
import Data.List.Extra as Export (chunksOf)

-- container types

import Data.Map as Export (Map)
import qualified Data.Map as Map
import Data.Set as Export (Set)
import qualified Data.Set as Set

-- io types

import GHC.Types as Export (IO)
import System.IO as Export (print, putStrLn, putStr, getLine, readFile, writeFile)

-- functions

import Data.Function as Export (id, const, flip, on, ($))

-- abstract algebra

import Control.Arrow as Export (Arrow (..), (<<<), (>>>))
import Control.Monad as Export (Monad (..))
import Data.Functor as Export (Functor (..))
import Data.Monoid as Export (Monoid (..))
import Data.Semigroup as Export (Semigroup (..))

-- * renames

type Int64 = P.Int
type Float32 = P.Float
type Float64 = P.Double

-- * syntax

infixl 9 .

{-# INLINE (.) #-}
(.) :: a -> (a -> b) -> b
x . f = f x

infixr 5 ++

{-# INLINE (++) #-}
(++) :: Monoid a => a -> a -> a
(++) = (<>)

infixl 7 //

{-# INLINE (//) #-}
(//) :: Integral a => a -> a -> a
(//) = div

-- infixl 7 /

-- {-# INLINE (/) #-}
-- (/) :: Integral a => a -> a -> Ratio a
-- (/) = (P.%)
