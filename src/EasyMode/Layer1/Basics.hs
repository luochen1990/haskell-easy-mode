module EasyMode.Layer1.Basics
(
    module Export,
    module EasyMode.Layer1.Basics
)
where

-- * basic types

import Data.Bool as Export (Bool(..), (&&), (||))
import Data.Text as Export (Text, pack, unpack)
import Data.Char as Export (Char)
import Data.ByteString as Export (ByteString)
--import Data.String as Export (String)
import Data.Text.Encoding as Export (encodeUtf8, decodeUtf8, decodeLatin1)

-- * number types

import GHC.Integer as Export (Integer)
import Data.Eq as Export (Eq(..))
import Data.Ord as Export (Ord(..))
import GHC.Num as Export (Num(..))
import GHC.Enum as Export (maxBound, minBound)
import GHC.Real as Export (fromIntegral)

-- * simple container types

import Data.Maybe as Export (Maybe(..), fromJust)
import Data.Either as Export (Either(..))
import Data.Either.Extra as Export (fromLeft', fromRight', mapLeft, mapRight)
import Data.Monoid as Export (Monoid(..), Endo(..))

-- * list types

import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Data.List.Extra as Export (chunksOf)
import Data.List as Export (iterate, unfoldr)
import Data.Foldable as Export (Foldable)

-- * container types

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map as Export (Map)
import Data.Set as Export (Set)

-- * io types

import GHC.Types as Export (IO)
import System.IO as Export (print)

-- * functions

import Data.Function as Export (id)

-- * abstract algebra

import Data.Semigroup as Export (Semigroup(..))
import Data.Monoid as Export (Monoid(..))
import Control.Arrow as Export (Arrow(..), (<<<), (>>>))
import Control.Monad as Export (Monad(..))

-- * syntax

infixl 9 .   

{-# INLINE (.) #-}
(.) :: a -> (a -> b) -> b
x . f = f x

