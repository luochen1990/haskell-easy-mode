module EasyMode.Layer3.List
(
  module EasyMode.Layer3.List,
  module Export
)
where

import EasyMode.Layer2
import Data.List as Export hiding (head, tail, last, take, drop, (!!))
import qualified Data.List as L
import GHC.Integer (Integer)

head :: Partial => [a] -> a
head = L.head

last :: Partial => [a] -> a
last = L.last

tail :: Partial => [a] -> [a]
tail = L.tail

take :: Integer -> [a] -> [a]
take n xs = L.take (ocast n) xs

drop :: Integer -> [a] -> [a]
drop n xs = L.drop (ocast n) xs

(!!) :: [a] -> Integer -> a
xs !! n = xs L.!! (ocast n)

