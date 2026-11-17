module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty

-- Реализуйте все представленные ниже классы (см. тесты)

instance (Show a) => Show (ReverseList a) where
    show as = "[" ++ joined' ++ "]"
        where joined  = foldMap ((',' :) . show) as
              joined' = drop 1 joined -- leading comma

instance (Eq a) => Eq (ReverseList a) where
    REmpty  == REmpty  = True
    (xs:<x) == (ys:<y) = x == y && xs == ys
    _       == _       = False

instance Semigroup (ReverseList a) where
    xs <> REmpty  = xs
    xs <> (ys:<y) = (xs <> ys):<y 
    
instance Monoid (ReverseList a) where
    mempty = REmpty
    
instance Functor ReverseList where
    fmap _ REmpty  = REmpty
    fmap f (xs:<x) = (fmap f xs):<(f x)
    
instance Applicative ReverseList where
    pure x = REmpty:<x
    REmpty  <*> xs = REmpty
    (fs:<f) <*> xs = (fs <*> xs) <> (fmap f xs)
    
instance Monad ReverseList where
    REmpty  >>= _ = REmpty
    (xs:<x) >>= f = (xs >>= f) <> (f x)

instance Foldable ReverseList where
    foldMap _ REmpty  = mempty
    foldMap f (xs:<x) = foldMap f xs <> f x
