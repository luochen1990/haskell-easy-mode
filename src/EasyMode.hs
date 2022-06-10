module EasyMode
(
    module Export,
    module EasyMode
)
where

import EasyMode.Layer3 as Export

-- this is for RebindableSyntax
--fromString :: String -> Text
--fromString = pack
--
--fromInteger :: Integer -> Integer
--fromInteger = id
--
--ifThenElse :: Bool -> a -> a -> a
--ifThenElse p x y = case p of True -> x; False -> y;

