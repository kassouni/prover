{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Proposition where


import Formula
import Structure (nand)
import Data.Foldable (Foldable(toList))
import Prelude hiding (and)
import BooleanAlgebra

data Proposition a = PNot (Proposition a) | POr (Proposition a) (Proposition a) | PVar a deriving Foldable

type PropAssignment a = a -> Bool

instance BooleanAlgebraic (Proposition String) where
    or = POr
    not = PNot
    bottom = and (BooleanAlgebra.not (PVar "x")) (PVar "x")

modifyAssignment :: Eq a => a -> Bool -> PropAssignment a -> PropAssignment a
modifyAssignment x t s v = if x == v then t else s v

allAssignments :: Eq a => [a] -> [PropAssignment a]
allAssignments [] = [const False]
allAssignments (x:xs) = let xsAssignment = allAssignments xs in
    (modifyAssignment x True <$> xsAssignment) ++ xsAssignment

evalAssignment :: Proposition a -> PropAssignment a -> Bool
evalAssignment (PVar x) s = s x
evalAssignment (POr α β) s = nand (evalAssignment α s) (evalAssignment β s)
evalAssignment (PNot α) s = Prelude.not (evalAssignment α s)

-- a proposition is a tautology if it is true for all assignments
-- e.g. a or not a
tautological :: Eq a => Proposition a -> Bool
tautological φ = all (evalAssignment φ) (allAssignments (toList φ))

propositionalize :: Show (Formula r f v) => Formula r f v -> Proposition String
propositionalize (And αs) = conjunction (propositionalize <$> αs)
propositionalize (Impl α β) = undefined 
propositionalize (Not α) = PNot (propositionalize α)
propositionalize φ@Forall {} = PVar $ show φ
propositionalize φ@Eq {} = PVar $ show φ
propositionalize φ@Rel {} = PVar $ show φ

propositionalConsequence :: Show (Formula r f v) => [Formula r f v] -> Formula r f v -> Bool
propositionalConsequence γ φ = tautological (implies (conjunction (propositionalize <$> γ)) (propositionalize φ))
