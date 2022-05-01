{-# LANGUAGE FlexibleInstances #-}
module Godel where

import Formula
import NumberTheory
import Data.Char (ord)
import Data.Numbers.Primes (primes)

-- this code is mostly useless since godel nubmers are too big to efficiently compute

-- data Tree a = Leaf a | Branch [Tree a] deriving (Show)

-- class GodelNumber a where
--     godel_tree :: a -> Tree Integer

-- instance GodelNumber NTTerm where
--     godel_tree (Fun Zero) = Branch [Leaf 9]
--     godel_tree (Fun (S t)) = Branch [Leaf 11, godel_tree t]
--     godel_tree (Fun (Add t1 t2)) = Branch [Leaf 13, godel_tree t1, godel_tree t2]
--     godel_tree (Fun (Mult t1 t2)) = Branch [Leaf 15, godel_tree t1, godel_tree t2]
--     godel_tree (Fun (Exp t1 t2)) = Branch [Leaf 17, godel_tree t1, godel_tree t2]
--     godel_tree (Var x) = Branch [Leaf (toInteger (ord x) * 2)]

-- instance GodelNumber NTFormula where
--     godel_tree (Eq t1 t2) = Branch [Leaf 7, godel_tree t1, godel_tree t2]
--     godel_tree (Not α) = Branch [Leaf 1, godel_tree α]
--     godel_tree (Or α β) = Branch [Leaf 3, godel_tree α, godel_tree β]
--     godel_tree (Forall x α) = Branch [Leaf 5, godel_tree (Var x :: NTTerm), godel_tree α]
--     godel_tree (Rel (Lt t1 t2)) = Branch [Leaf 19, godel_tree t1, godel_tree t2]

-- evalGT :: Tree Integer -> Integer
-- evalGT (Leaf n) = n
-- evalGT (Branch nodes) = product $ zipWith (^) primes (map (succ . evalGT) nodes)  

-- godelNumber :: NTFormula -> Integer
-- godelNumber = evalGT . godel_tree

-- zeroEqualsZero :: NTFormula 
-- zeroEqualsZero = Eq (Fun Zero) (Fun Zero)