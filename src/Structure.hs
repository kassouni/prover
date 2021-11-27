{-# LANGUAGE NamedFieldPuns #-}

module Structure where

import Numeric.Natural
import Formula (Symbol, Term (..), Formula(..))

class Enumerable u where
    enumerated :: [u]

data Structure u r f = Structure {
    functions :: f u -> u,
    relations :: r u -> Bool
}

type VariableAssigment u = Symbol -> u

nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

termAssignent :: Functor f => Structure u r f -> VariableAssigment u -> Term f -> u
termAssignent Structure{functions, relations} sub (Var x) = sub x
termAssignent struct@Structure{functions, relations} sub (Fun f) = functions (termAssignent struct sub <$> f)

assignmentModifier :: VariableAssigment u -> Symbol -> u -> VariableAssigment u
assignmentModifier sub x a v = if v==x then a  else sub v

-- UNDECIDABLE!!!!
satisfies :: Enumerable u => Eq u => Functor r => Functor f => Structure u r f -> VariableAssigment u -> Formula r (Term f) -> Bool
satisfies struct@Structure{functions, relations} sub (Eq t1 t2) = 
    termAssignent struct sub t1 == termAssignent struct sub t2
satisfies struct@Structure{functions, relations} sub (Rel r) = 
    relations (termAssignent struct sub <$> r)
satisfies struct@Structure{functions, relations} sub (Or α β) = 
    satisfies struct sub α || satisfies struct sub β
satisfies struct@Structure{functions, relations} sub (Not φ) = 
    not $ satisfies struct sub φ
satisfies struct@Structure{functions, relations} sub (Forall x α) = 
    and [satisfies struct (assignmentModifier sub x a) α | a <- enumerated]


