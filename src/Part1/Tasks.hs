module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed (mod')
import Data.List (sort)

eps = 1e-5
reduceAngle x = (x + pi) `mod'` (2*pi) - pi 

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum terms'
    where x' = reduceAngle x
          terms' = takeWhile ((> eps) . abs) terms
          terms = x':(zipWith nxt [0..] terms)
          nxt n t = -t * x'^2 / (2*n + 2) / (2*n + 3)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum terms'
    where x' = reduceAngle x
          terms' = takeWhile ((> eps) . abs) terms
          terms = 1.0:(zipWith nxt [0..] terms)
          nxt n t = -t * x'^2 / (2*n + 1) / (2*n + 2)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = fst $ until ((== 0) . snd) nxt (abs a, abs b)
    where nxt (a, b) = (b, a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y = 1 <= m && m <= 12 && 1 <= d && d <= daysInMonth m y
    where isLeap y = y `mod` 400 == 0 || (y `mod` 4 == 0 && y `mod` 100 /= 0)
          daysInMonth 2 y
            | isLeap y  = 29
            | otherwise = 28
          daysInMonth m _
            | m `elem` [4, 6, 9, 11] = 30
            | otherwise = 31

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x p = iterate (*x) 1 !! fromIntegral p

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = length divs == 2
    where divs = filter ((== 0) . (n `mod`)) [1..n]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea [] = 0
shapeArea ps = (abs $ sum terms) / 2
    where terms = zipWith term ps (tail $ cycle ps)
          term (x1, y1) (x2, y2) = x1 * y2 - y1 * x2 

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a + b + c <= 2*m        = impossible
    | a^2 + b^2 + c^2 < 2*m^2 = obtuse
    | a^2 + b^2 + c^2 > 2*m^2 = acute
    | otherwise               = right
    where
        m = maximum [a, b, c]
        obtuse     = 0
        acute      = 1
        right      = 2
        impossible = -1
