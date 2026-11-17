module Part2.Tasks where

import Util(notImplementedYet)
import Data.Function (fix)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar var replacement = fix replace'
   where replace' recur (BinaryTerm op lhv rhv) = BinaryTerm op (recur lhv) (recur rhv)
         replace' recur term
            | term == (Variable var) = replacement
            | otherwise              = term

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate term = case evaluate' term of
   Just x  -> IntConstant x
   Nothing -> term
   where evaluate' (IntConstant val) = Just val
         evaluate' (BinaryTerm op lhv rhv) = do
            a <- evaluate' lhv
            b <- evaluate' rhv
            return $ apply op a b
         evaluate' (Variable _) = Nothing
         apply Minus = (-)
         apply Plus  = (+)
         apply Times = (*)
