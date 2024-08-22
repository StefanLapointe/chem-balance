module LinearAlgebra (
    Vector
  , toVector
  , fromVector
  , Matrix
  , toMatrix
  , fromMatrix
  , rref
  , solveSystem
    ) where

import Data.Array

type Vector = Array Int Rational

toVector :: [Rational] -> Vector
toVector xs = listArray (1, length xs) xs

fromVector :: Vector -> [Rational]
fromVector = elems

type Matrix = Array (Int, Int) Rational

toMatrix :: [[Rational]] -> Matrix
toMatrix rows = listArray ((1, 1), (n, m)) $ concat rows
  where
    n = length rows
    m = length $ head rows

fromMatrix :: Matrix -> [[Rational]]
fromMatrix m = rowList $ elems m
  where
    rowList :: [Rational] -> [[Rational]]
    rowList [] = []
    rowList xs = row : rowList rest
      where
        (row, rest) = splitAt (nCols m) xs

dims :: Matrix -> (Int, Int)
dims = snd . bounds

nRows :: Matrix -> Int
nRows = fst . dims

nCols :: Matrix -> Int
nCols = snd . dims

-- The reduced row echelon form of a given matrix.
rref :: Matrix -> Matrix
rref = elimination 1 1

-- Modified Gaussian elimination.
-- The goal here is to avoid using (//) too often.
-- This should take O(m^2n) time for an m x n matrix.
elimination :: Int -> Int -> Matrix -> Matrix
-- Out of rows. Done.
elimination row _ m
  | row > nRows m = m
-- Out of columns. Done.
elimination _ col m
  | col > nCols m = m
-- General case. Recurse.
elimination row col m =
  case filter (\i -> m ! (i, col) /= 0) [row..nRows m] of
    -- No pivot found.
    [] -> elimination row (col + 1) m
    -- Pivot found.
    (row' : _) -> elimination (row + 1) (col + 1) $ m' // updates
      where
        -- Bring the pivot to the top and make it 1.
        m' = rowMultiply (1 / m ! (row', col)) row $ rowSwap row row' m
        -- This is where all of the cancellation happens.
        updates = [((i, j), m' ! (i, j) - m' ! (i, col) * m' ! (row, j))
                  | i <- [1..row - 1] ++ [row + 1..nRows m]
                  , j <- [col..nCols m]
                  ]

rowSwap :: Int -> Int -> Matrix -> Matrix
rowSwap i1 i2 m = ixmap (bounds m) rowSwapMap m
  where
    rowSwapMap :: (Int, Int) -> (Int, Int)
    rowSwapMap (i, j)
      | i == i1 = (i2, j)
      | i == i2 = (i1, j)
      | otherwise = (i, j)

rowMultiply :: Rational -> Int -> Matrix -> Matrix
rowMultiply c i m = m // [((i, j), c * m ! (i, j)) | j <- [1..nCols m]]

solveSystem :: Matrix -> Maybe Vector
solveSystem augmentedMatrix = extractSolution $ rref $ augmentedMatrix
  where
    extractSolution :: Matrix -> Maybe Vector
    extractSolution m
      | nRows augmentedMatrix < nCols augmentedMatrix - 1 = Nothing
      | m ! (nCols m - 1, nCols m - 1) == 0 = Nothing
      | nRows m >= nCols m && m ! (nCols m, nCols m) /= 0 = Nothing
      | otherwise = Just $ ixmap (1, nCols m - 1) (\i -> (i, nCols m)) m
