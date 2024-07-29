module EasyMode (
    module EasyMode,
) where

import Control.Arrow as EasyMode
import Control.Monad as EasyMode
import Data.Data as EasyMode hiding (cast)
import Data.Either as EasyMode
import qualified Data.Either as Either
import Data.Foldable as EasyMode hiding (foldl1, foldr1, length, toList)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe as EasyMode
import qualified Data.Maybe as Maybe
import Data.Monoid as EasyMode
import qualified Data.Set as Set
import Data.Traversable as EasyMode
import Data.Typeable as EasyMode hiding (cast)
import EasyMode.Layers.L3 as EasyMode

-- import Prelude hiding (fromString, fromInteger)

default (Integer, Rational, Float64, Text)


---- this is for RebindableSyntax
-- fromString :: String -> Text
-- fromString = pack
--
-- fromInteger :: Integer -> Integer
-- fromInteger = id

-- ifThenElse :: Bool -> a -> a -> a
-- ifThenElse p x y = case p of True -> x; False -> y;
