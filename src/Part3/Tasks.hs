module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n):(finc f (n + 1))

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f a = a:(ff f (f a))

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq ns = maxBy (count digits) [0..9]
    where maxBy f = foldl1 $ \x y -> if f x < f y then y else x
          count [] x = 0
          count (y:ys) x
            | x == y    = 1 + (count ys x)
            | otherwise = count ys x
          digits  = fmap charToDigit $ foldl (++) [] $ fmap show ns
          charToDigit c = (fromEnum c) - (fromEnum '0')

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (a:as) = a:(uniq $ filter (/= a) as)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f [] = []
grokBy f as = zip ks $ fmap konly ks
    where ks = uniq $ fmap f as
          konly k = filter ((== k) . f) as
