module EasyMode
(
    module Export,
    module EasyMode
)
where

import EasyMode.Layer3 as Export
import Data.Maybe as Export
import Data.Either as Export
import Data.Monoid as Export
import Data.Foldable as Export hiding (toList, foldr1, foldl1, length)
import Data.Traversable as Export
import Control.Monad as Export
import Control.Arrow as Export

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

