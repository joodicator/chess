module ListMap(Map, empty, lookup, insert, delete, toList) where

import qualified Data.List as L
import Prelude hiding (lookup)
import Data.Function

type Map k a = [(k,a)] -- Sorted in ascending order.

empty :: Map k a
empty = []

lookup :: Ord k => k -> Map k a -> Maybe a
lookup k ((k',x):es) = case compare k k' of
    GT -> lookup k es
    EQ -> Just x
    LT -> Nothing
lookup _ _ = Nothing

insert :: Ord k => k -> a -> Map k a -> Map k a
insert k x es@(e@(k',_):es') = case compare k k' of
    GT -> e : insert k x es'
    EQ -> es
    LT -> (k,x) : es
insert k x _ = [(k,x)]

delete :: Ord k => k -> Map k a -> Map k a
delete k es@(e@(k',_):es') = case compare k k' of
    GT -> e : delete k es'
    EQ -> es'
    LT -> es
delete _ es = es

toList = id
