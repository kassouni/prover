{-# LANGUAGE DeriveFoldable #-}
module Proposition where


import Formula
import Structure (nand)
import Data.Foldable (Foldable(toList))

data Proposition a = PNot (Proposition a) | POr (Proposition a) (Proposition a) | PVar a deriving Foldable

type PropAssignment a = a -> Bool

modifyAssignment :: Eq a => a -> Bool -> PropAssignment a -> PropAssignment a
modifyAssignment x t s v = if x == v then t else s v

allAssignments :: Eq a => [a] -> [PropAssignment a]
allAssignments [] = [const False]
allAssignments (x:xs) = let xsAssignment = allAssignments xs in
    (modifyAssignment x True  <$> xsAssignment) ++ xsAssignment

evalAssignment :: Proposition a -> PropAssignment a -> Bool
evalAssignment (PVar x) s = s x
evalAssignment (POr α β) s = nand (evalAssignment α s) (evalAssignment β s)
evalAssignment (PNot α) s = not (evalAssignment α s)


tautological :: Eq a => Proposition a -> Bool
tautological φ = all (evalAssignment φ) (allAssignments (toList φ))

propositionalize :: Formula r t -> Proposition String
propositionalize (Or α β) = POr (propositionalize α) (propositionalize β)
propositionalize (Not α) = PNot (propositionalize α)
propositionalize φ@Forall {} = PVar $ show φ
propositionalize φ@Eq {} = PVar $ show φ
propositionalize φ@Rel {} = PVar $ show φ
