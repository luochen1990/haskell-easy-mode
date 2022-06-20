module EasyMode.Layer3.List
(
  (L.++), -- :: [a] -> [a] -> [a]
  -- L.head, -- :: [a] -> a
  -- L.last, -- :: [a] -> a
  -- L.tail, -- :: [a] -> [a]
  L.init, -- :: [a] -> [a]
  L.uncons, -- :: [a] -> Maybe (a, [a])
  L.singleton, -- :: a -> [a]
  L.null, -- :: Foldable t => t a -> Bool
  -- L.length, -- :: Foldable t => t a -> Int
  L.map, -- :: (a -> b) -> [a] -> [b]
  L.reverse, -- :: [a] -> [a]
  L.intersperse, -- :: a -> [a] -> [a]
  L.intercalate, -- :: [a] -> [[a]] -> [a]
  L.transpose, -- :: [[a]] -> [[a]]
  L.subsequences, -- :: [a] -> [[a]]
  L.permutations, -- :: [a] -> [[a]]
  L.foldl, -- :: Foldable t => (b -> a -> b) -> b -> t a -> b
  L.foldl', -- :: Foldable t => (b -> a -> b) -> b -> t a -> b
  -- L.foldl1, -- :: Foldable t => (a -> a -> a) -> t a -> a
  -- L.foldl1', -- :: (a -> a -> a) -> [a] -> a
  L.foldr, -- :: Foldable t => (a -> b -> b) -> b -> t a -> b
  -- L.foldr1, -- :: Foldable t => (a -> a -> a) -> t a -> a
  L.concat, -- :: Foldable t => t [a] -> [a]
  L.concatMap, -- :: Foldable t => (a -> [b]) -> t a -> [b]
  L.and, -- :: Foldable t => t Bool -> Bool
  L.or, -- :: Foldable t => t Bool -> Bool
  L.any, -- :: Foldable t => (a -> Bool) -> t a -> Bool
  L.all, -- :: Foldable t => (a -> Bool) -> t a -> Bool
  L.sum, -- :: (Foldable t, Num a) => t a -> a
  L.product, -- :: (Foldable t, Num a) => t a -> a
  L.maximum, -- :: forall a. (Foldable t, Ord a) => t a -> a
  L.minimum, -- :: forall a. (Foldable t, Ord a) => t a -> a
  L.scanl, -- :: (b -> a -> b) -> b -> [a] -> [b]
  L.scanl', -- :: (b -> a -> b) -> b -> [a] -> [b]
  L.scanl1, -- :: (a -> a -> a) -> [a] -> [a]
  L.scanr, -- :: (a -> b -> b) -> b -> [a] -> [b]
  L.scanr1, -- :: (a -> a -> a) -> [a] -> [a]
  L.mapAccumL, -- :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
  L.mapAccumR, -- :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
  L.iterate, -- :: (a -> a) -> a -> [a]
  L.iterate', -- :: (a -> a) -> a -> [a]
  L.repeat, -- :: a -> [a]
  -- L.replicate, -- :: Int -> a -> [a]
  L.cycle, -- :: [a] -> [a]
  L.unfoldr, -- :: (b -> Maybe (a, b)) -> b -> [a]
  -- L.take, -- :: Int -> [a] -> [a]
  -- L.drop, -- :: Int -> [a] -> [a]
  -- L.splitAt, -- :: Int -> [a] -> ([a], [a])
  L.takeWhile, -- :: (a -> Bool) -> [a] -> [a]
  L.dropWhile, -- :: (a -> Bool) -> [a] -> [a]
  L.dropWhileEnd, -- :: (a -> Bool) -> [a] -> [a]
  L.span, -- :: (a -> Bool) -> [a] -> ([a], [a])
  L.break, -- :: (a -> Bool) -> [a] -> ([a], [a])
  L.stripPrefix, -- :: Eq a => [a] -> [a] -> Maybe [a]
  -- L.group, -- :: Eq a => [a] -> [[a]]
  L.inits, -- :: [a] -> [[a]]
  L.tails, -- :: [a] -> [[a]]
  L.isPrefixOf, -- :: Eq a => [a] -> [a] -> Bool
  L.isSuffixOf, -- :: Eq a => [a] -> [a] -> Bool
  L.isInfixOf, -- :: Eq a => [a] -> [a] -> Bool
  L.isSubsequenceOf, -- :: Eq a => [a] -> [a] -> Bool
  L.elem, -- :: (Foldable t, Eq a) => a -> t a -> Bool
  L.notElem, -- :: (Foldable t, Eq a) => a -> t a -> Bool
  L.lookup, -- :: Eq a => a -> [(a, b)] -> Maybe b
  L.find, -- :: Foldable t => (a -> Bool) -> t a -> Maybe a
  L.filter, -- :: (a -> Bool) -> [a] -> [a]
  L.partition, -- :: (a -> Bool) -> [a] -> ([a], [a])
  -- (L.!!), -- :: [a] -> Int -> a
  -- L.elemIndex, -- :: Eq a => a -> [a] -> Maybe Int
  -- L.elemIndices, -- :: Eq a => a -> [a] -> [Int]
  -- L.findIndex, -- :: (a -> Bool) -> [a] -> Maybe Int
  -- L.findIndices, -- :: (a -> Bool) -> [a] -> [Int]
  L.zip, -- :: [a] -> [b] -> [(a, b)]
  L.zip3, -- :: [a] -> [b] -> [c] -> [(a, b, c)]
  L.zip4, -- :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
  L.zip5, -- :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
  L.zip6, -- :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
  L.zip7, -- :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
  L.zipWith, -- :: (a -> b -> c) -> [a] -> [b] -> [c]
  L.zipWith3, -- :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
  L.zipWith4, -- :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
  L.zipWith5, -- :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
  L.zipWith6, -- :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
  L.zipWith7, -- :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
  L.unzip, -- :: [(a, b)] -> ([a], [b])
  L.unzip3, -- :: [(a, b, c)] -> ([a], [b], [c])
  L.unzip4, -- :: [(a, b, c, d)] -> ([a], [b], [c], [d])
  L.unzip5, -- :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
  L.unzip6, -- :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
  L.unzip7, -- :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
  -- L.lines, -- :: String -> [String]
  -- L.words, -- :: String -> [String]
  -- L.unlines, -- :: [String] -> String
  -- L.unwords, -- :: [String] -> String
  L.nub, -- :: Eq a => [a] -> [a]
  L.delete, -- :: Eq a => a -> [a] -> [a]
  (L.\\), -- :: Eq a => [a] -> [a] -> [a]
  L.union, -- :: Eq a => [a] -> [a] -> [a]
  L.intersect, -- :: Eq a => [a] -> [a] -> [a]
  L.sort, -- :: Ord a => [a] -> [a]
  L.sortOn, -- :: Ord b => (a -> b) -> [a] -> [a]
  L.insert, -- :: Ord a => a -> [a] -> [a]
  L.nubBy, -- :: (a -> a -> Bool) -> [a] -> [a]
  L.deleteBy, -- :: (a -> a -> Bool) -> a -> [a] -> [a]
  L.deleteFirstsBy, -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  L.unionBy, -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  L.intersectBy, -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
  -- L.groupBy, -- :: (a -> a -> Bool) -> [a] -> [[a]]
  L.sortBy, -- :: (a -> a -> Ordering) -> [a] -> [a]
  L.insertBy, -- :: (a -> a -> Ordering) -> a -> [a] -> [a]
  L.maximumBy, -- :: Foldable t => (a -> a -> Ordering) -> t a -> a
  L.minimumBy, -- :: Foldable t => (a -> a -> Ordering) -> t a -> a
  L.genericLength, -- :: Num i => [a] -> i
  L.genericTake, -- :: Integral i => i -> [a] -> [a]
  L.genericDrop, -- :: Integral i => i -> [a] -> [a]
  L.genericSplitAt, -- :: Integral i => i -> [a] -> ([a], [a])
  L.genericIndex, -- :: Integral i => [a] -> i -> a
  L.genericReplicate, -- :: Integral i => i -> a -> [a]
  module EasyMode.Layer3.List
)
where

import EasyMode.Layer2
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import GHC.Integer (Integer)
import Data.Bool (not)
import Data.Functor (fmap)
import Data.Hashable (Hashable)
import Data.Foldable (toList)

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

groupBy :: (Hashable k, Foldable f) => (a -> k) -> f a -> M.HashMap k [a]
groupBy key xs = M.fromListWith (L.++) (L.map (\x -> (key x, L.singleton x)) (toList xs))

groupContinuously :: (Foldable f, Eq a) => f a -> [[a]]
groupContinuously xs = L.group (toList xs)

groupContinuouslyBy :: (Foldable f, Eq a) => (a -> a -> Bool) -> f a -> [[a]]
groupContinuouslyBy eq xs = L.groupBy eq (toList xs)

foldl1 :: (Partial, Foldable t) => (a -> a -> a) -> t a -> a
foldl1 op xs = assume (not (L.null xs)) (L.foldl1 op xs)

foldl1' :: Partial => (a -> a -> a) -> [a] -> a
foldl1' op xs = assume (not (L.null xs)) (L.foldl1' op xs)

foldr1 :: (Partial, Foldable t) => (a -> a -> a) -> t a -> a
foldr1 op xs = assume (not (L.null xs)) (L.foldr1 op xs)

length :: Foldable t => t a -> Integer
length xs = cast (L.length xs)

replicate :: Integer -> a -> [a]
replicate n x = L.replicate (ocast n) x

splitAt :: Integer -> [a] -> ([a], [a])
splitAt n xs = L.splitAt (ocast n) xs

elemIndex :: Eq a => a -> [a] -> Maybe Integer
elemIndex x xs = fmap cast (L.elemIndex x xs)

elemIndices :: Eq a => a -> [a] -> [Integer]
elemIndices x xs = L.map cast (L.elemIndices x xs)

findIndex :: (a -> Bool) -> [a] -> Maybe Integer
findIndex p xs = fmap cast (L.findIndex p xs)

findIndices :: (a -> Bool) -> [a] -> [Integer]
findIndices p xs = L.map cast (L.findIndices p xs)

lines :: Text -> [Text]
lines = T.lines

words :: Text -> [Text]
words = T.words

unlines :: [Text] -> Text
unlines = T.unlines

unwords :: [Text] -> Text
unwords = T.unwords

