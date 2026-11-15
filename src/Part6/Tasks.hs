{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       -- all dimensions/index pairs are (row, col) / (height, width)
       dimensions :: mx -> (Int, Int)
       construct :: (Int -> Int -> Int) -> Int -> Int -> mx
       get :: mx -> Int -> Int -> Int

height :: Matrix mx => mx -> Int
height = fst . dimensions

width :: Matrix mx => mx -> Int
width  = snd . dimensions

removeRow :: Matrix mx => Int -> mx -> mx
removeRow r a = construct gen m n
       where gen i j = get a (if i < r then i else i + 1) j
             m = height a - 1
             n = width a

removeCol :: Matrix mx => Int -> mx -> mx
removeCol c a = construct gen m n
       where gen i j = get a i (if j < c then j else j + 1)
             m = height a
             n = width a - 1

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       dimensions _ = (1, 1)
       construct f _ _ = f 0 0
       get v 0 0 = v
       get _ _ _ = error "out of bounds"

-- [[Int]] - список столбцов
instance Matrix [[Int]] where
       dimensions []   = (0, 0)
       dimensions cols = (minimum $ fmap length cols, length cols)
       construct f h w = fmap col [0..w-1]
              where col j = fmap (flip f $ j) [0..h-1]
       get cols i j = (cols !! j) !! i
       
instance Matrix (SparseMatrix Int) where
       dimensions a = (sparseMatrixHeight a, sparseMatrixWidth a)
       construct f h w = SparseMatrix { sparseMatrixWidth = w, sparseMatrixHeight = h, sparseMatrixElements = elements } 
              where elements = fromList [((i, j), v) | i <- [0..h-1], j <- [0..w-1], let v = f i j, v /= 0]
       get m i j = findWithDefault 0 (i, j) (sparseMatrixElements m)

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = construct (\c r -> if c == r then 1 else 0) w w

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero = construct (const . const 0)

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix a b = construct c l n
       where (l, m) = dimensions a
             (_, n) = dimensions b
             c i j  = sum $ fmap (\r -> (get a i r) * (get b r j)) [0..m-1]

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant a
       | m /= n    = error "determinant is undefined for non-square matricies"
       | n == 0    = 1
       | otherwise = sum $ fmap (\j -> (-1)^(i+j) * (get a i j) * (minor i j a)) [0..n-1]
       where (m, n) = dimensions a
             minor i' j' = determinant . (removeRow i') . (removeCol j')       
             i = 0
