module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z []     = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr ((:) . f) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr ((++) . f) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr filter' []
    where filter' a = if p a then (a :) else id

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr part' ([], [])
    where part' a = if p a then truth (a :) else false (a :)
          truth g (ts, fs) = (g ts, fs)
          false g (ts, fs) = (ts, g fs)
