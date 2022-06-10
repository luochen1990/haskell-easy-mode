module EasyMode
(
    module Export,
    module EasyMode
)
where

import EasyMode.Basics as Export
import EasyMode.Cast as Export
import EasyMode.Error as Export
import EasyMode.List as Export

import Data.Text (Text, pack, unpack)
import Prelude (String, Integer, Bool(..), id)

-- this is for RebindableSyntax
--fromString :: String -> Text
--fromString = pack
--
--fromInteger :: Integer -> Integer
--fromInteger = id
--
--ifThenElse :: Bool -> a -> a -> a
--ifThenElse p x y = case p of True -> x; False -> y;

