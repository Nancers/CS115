-- Nancy Cao
-- Assignment 2
-- PART A: Implementing the Priority Queue

module PriorityQueue(Pqueue,
                     empty,
                     isEmpty,
                     insert,
                     findMin,
                     deleteMin,
                     popMin,
                     fromList,
                     isValid)
where

-- Datatype for priority queues, parameterized around an element
-- type a.
-- The element type should be an instance of the Ord type class.
data Pqueue a = Leaf | Node a Int (Pqueue a) (Pqueue a)
    deriving (Show, Eq)

-- An empty priority queue storing values of type a
empty :: Pqueue a
empty = Leaf

-- Return True if the queue is empty.
isEmpty :: Pqueue a -> Bool
isEmpty Leaf = True
isEmpty _ = False

-- An integer rank of the priority queue arg
rank :: Pqueue a -> Int
rank Leaf = 0
rank (Node _ rank _ _) = rank

-- Takes two priority queues and merges them, returning a new
-- priority queue.
merge :: Ord a => Pqueue a -> Pqueue a -> Pqueue a
merge Leaf q = q
merge q Leaf = q
merge a@(Node a1 rkA lA rA) b@(Node b1 rkB lB rB)
      | a1 < b1 && (rank (merge rA b)) < (rank lA) = Node a1 (rank (merge rA b) + 1) lA (merge rA b)
      | a1 < b1 && (rank lA) <= (rank (merge rA b)) = Node a1 (rank lA + 1) (merge rA b) lA
      | a1 >= b1 && (rank lB) < (rank (merge a rB)) = Node b1 (rank lB + 1) (merge a rB) lB
      | otherwise = Node b1 (rank (merge a rB) + 1) lB (merge a rB)

-- Insert an item into a priority queue.
insert :: Ord a => a -> Pqueue a -> Pqueue a
insert a q = merge (Node a 1 Leaf Leaf) q

-- Find the minimum-valued element in a priority queue if possible.
findMin :: Ord a => Pqueue a -> Maybe a
findMin Leaf = Nothing
findMin (Node a _ _ _) = Just a

-- Delete the minimum element from a priority queue if possible.
deleteMin :: Ord a => Pqueue a -> Maybe (Pqueue a)
deleteMin Leaf = Nothing
deleteMin (Node _ _ l r) = Just (merge l r)

-- Remove the minimum element if possible and return it, 
-- along with the rest of the priority queue.
popMin :: Ord a => Pqueue a -> Maybe (a, Pqueue a)
popMin Leaf = Nothing
popMin (Node a _ l r) = Just (a, (merge l r))

-- Convert an unordered list into a priority queue.
fromList :: Ord a => [a] -> Pqueue a
fromList l = foldr (insert) Leaf l

-- Validate the internal structure of the priority queue.
isValid :: Ord a => Pqueue a -> Bool
isValid Leaf = True
isValid (Node a rk l r) | rk <= 0 = False
                        | (rank l) < (rank r) = False
                        | rk - 1 /= (rank r) = False
                        | (findMin l /= Nothing && Just a > findMin l) && (findMin r /= Nothing && Just a > findMin r) = False
                        | otherwise = (isValid l) && (isValid r)

