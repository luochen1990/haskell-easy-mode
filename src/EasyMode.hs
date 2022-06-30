module EasyMode
(
    module EasyMode
)
where

import EasyMode.Layers.L3 as EasyMode
import Data.Maybe as EasyMode
import Data.Either as EasyMode
import Data.Monoid as EasyMode
import Data.Foldable as EasyMode hiding (toList, foldr1, foldl1, length)
import Data.Traversable as EasyMode
import Control.Monad as EasyMode
import Control.Arrow as EasyMode
import Data.Typeable as EasyMode hiding (cast)
import Data.Data as EasyMode hiding (cast)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either

default (Integer, Rational, Float64, Text)

-- this is for RebindableSyntax
--fromString :: String -> Text
--fromString = pack
--
--fromInteger :: Integer -> Integer
--fromInteger = id
--
--ifThenElse :: Bool -> a -> a -> a
--ifThenElse p x y = case p of True -> x; False -> y;

-- f :: Partial => Text -> Integer
-- f s = buckpassing (ocast s)
-- 
-- g :: Partial => Text -> Integer
-- g s = buckpassing (f (s <> "0"))

