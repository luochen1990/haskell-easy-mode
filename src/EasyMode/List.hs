module EasyMode.List
(
  module EasyMode.List,
  module Export
)
where

import Data.List as Export hiding (head, tail, last)
import qualified Data.List as L

import EasyMode.Error

head :: Partial => [a] -> a
head = L.head

last :: Partial => [a] -> a
last = L.last

tail :: Partial => [a] -> [a]
tail = L.tail

