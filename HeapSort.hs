-- Nancy Cao
-- Assignment 2
-- PART B: Implementing the Heapsort Algorithm

module HeapSort where

import qualified PriorityQueue as Q

-- Recursively finds the minimum; returns a list from greatest to
-- lowest value.
minList :: Ord a => [a] -> Maybe (a, Q.Pqueue a) -> [a]
minList l Nothing = l
minList l (Just (x, y)) = minList (x:l) (Q.popMin y)

-- Sorts the priority queue using the heapsort algorithm.
sort :: Ord a => [a] -> [a]
sort [] = []
sort l = reverse (minList [] (Q.popMin (Q.fromList l)))
