-- Nancy Cao
-- Assignment 3
-- Part C: Sparse Matrices

module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)

-- Question 1

-- sparseMatrix <list of index/element pairs> <bounds> -> sparse matrix
sparseMatrix :: (Eq a, Num a) => [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a
sparseMatrix list bounds | (fst bounds) > 0 && (snd bounds) > 0 = SM bounds (findRows (filterZero list) (fst bounds) S.empty)
                                                                            (findCols (filterZero list) (snd bounds) S.empty)
                                                                            (createMatrix (filterZero list) M.empty)
                         | otherwise  = error "Bounds are invalid."

-- Filters out elements that are 0
filterZero :: (Eq a, Num a) => [((Integer, Integer), a)] -> [((Integer, Integer), a)]
filterZero list = [(x, y) | (x, y) <- list, y /= 0]

-- Obtain row indices without zeros
findRows :: [((Integer, Integer), a)] -> Integer -> S.Set Integer -> S.Set Integer
findRows [] _ set = set
findRows (x:xs) bound set | (fst (fst x)) > bound || (fst (fst x)) < 1 = error "Row index out of bounds."
                          | otherwise = findRows xs bound (S.insert (fst (fst x)) set)

-- Obtain column indices without zeros
findCols :: [((Integer, Integer), a)] -> Integer -> S.Set Integer -> S.Set Integer
findCols [] _ set = set
findCols (x:xs) bound set | (snd (fst x)) > bound || (snd (fst x)) < 1 = error "Col index out of bounds."
                          | otherwise = findCols xs bound (S.insert (snd (fst x)) set)

-- Creates the matrix
createMatrix :: [((Integer, Integer), a)] -> (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a)
createMatrix [] matrix = matrix
createMatrix (x:xs) matrix = (createMatrix xs (M.insert ((fst (fst x)), (snd (fst x))) (snd x) matrix))

-- Question 2

-- Add two sparse matrices
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
addSM (SM bA _ _ vA) (SM bB _ _ vB) | bA /= bB = error "Matrix dimensions don't match."
                                    | otherwise = sparseMatrix (M.toList (M.unionWith (+) vA vB)) bA

-- Question 3

-- Negates a sparse matrix
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b r c vals) = (SM b r c (M.map (negate) vals))

-- Question 4

-- Subtracts one sparse matrix from another
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM a@(SM bA _ _ _) b@(SM bB _ _ _) | bA /= bB = error "Matrix dimensions don't match."
                                      | otherwise = (addSM a (negateSM b))

-- Question 5

-- Multiplies two matrices
mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM bA rA _ vA) (SM bB _ cB vB) | (snd bA) /= (fst bB) = error "Matrix dimensions don't match."
                                      | otherwise = sparseMatrix [((i, j), dotProd (M.filterWithKey (\(r, _) _ -> r == i) vA) (M.filterWithKey (\(_, c) _ -> c == j) vB)) | i <- S.toList rA, j <- S.toList cB] ((fst bA), (snd bB))

-- Calculates dot product of two vectors
dotProd :: (Eq a, Num a) => (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a) -> a
dotProd a b = sum (M.elems (M.intersectionWith (*) (M.mapKeys (\(_, c) -> c) a) (M.mapKeys (\(r, _) -> r) b)))

-- Question 6

-- Retreives a value from a sparse matrix given row and column
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM b _ _ v) (x, y) | x > (fst b) || x < 1 = error "Invalid row."
                          | y > (snd b) || y < 1 = error "Invalid column."
                          | otherwise = (M.findWithDefault 0 (x, y) v)

-- Returns number of rows in a sparse matrix
rowsSM :: SparseMatrix a -> Integer
rowsSM (SM bounds _ _ _) = (fst bounds)

-- Returns the number of columns in a sparse matrix
colsSM :: SparseMatrix a -> Integer
colsSM (SM bounds _ _ _) = (snd bounds)

-- Question 7

(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) a b = addSM a b

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) a b = subSM a b

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) a b = (mulSM a b)

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) a index = getSM a index

-- Question 8

-- The minimal definitions needed for the num type class are +,
-- *, abs, signum, fromInteger, and the negate function. Here for the
-- sparse matrix we have defined +, *, and the negate function.
-- It doesn't really make sense to have abs or signum defined for
-- sparse matrices since we can't really take an absolute value
-- of a sparse matrix or find the sign of a sparse matrix, since the
-- matrix contains multiple values.
