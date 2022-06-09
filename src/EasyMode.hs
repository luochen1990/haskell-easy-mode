module EasyMode
(
    module Export,
    fromString,
    fromInteger,
    ifThenElse,
)
where

import EasyMode.Cast as Export

import Data.Text (Text, pack, unpack)
import Prelude (String, Integer, Bool(..), id)

-- this is for RebindableSyntax
fromString :: String -> Text
fromString = pack

fromInteger :: Integer -> Integer
fromInteger = id

ifThenElse :: Bool -> a -> a -> a
ifThenElse p x y = case p of True -> x; False -> y;

