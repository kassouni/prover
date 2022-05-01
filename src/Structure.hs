{-# LANGUAGE NamedFieldPuns #-}

module Structure where

import Numeric.Natural
import Formula (Term (..), Formula(..))

(-->) :: Bool -> Bool -> Bool
(-->) True False = False
(-->) _ _ = True



class Enumerable u where
    enumerated :: [u]

data Structure u r f = Structure {
    functions :: f u -> u,
    relations :: r u -> Bool
}

type VariableAssigment v u = v -> u

nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

termAssignent :: Functor f => Structure u r f -> (v -> u) -> Term f v -> u
termAssignent Structure{functions, relations} sub (Var x) = sub x
termAssignent struct@Structure{functions, relations} sub (Fun f) = functions (termAssignent struct sub <$> f)

assignmentModifier :: Eq v => (v -> u) -> v -> u -> (v -> u)
assignmentModifier sub x a v = if v==x then a  else sub v

-- UNDECIDABLE!!!!
satisfies :: Enumerable u => Eq u => Eq v => Functor r => Functor f => Structure u r f -> (v -> u) -> Formula r f v -> Bool
satisfies struct@Structure{functions, relations} sub (Eq t1 t2) =
    termAssignent struct sub t1 == termAssignent struct sub t2
satisfies struct@Structure{functions, relations} sub (Rel r) =
    relations (termAssignent struct sub <$> r)
satisfies struct@Structure{functions, relations} sub (And αs) =
    all (satisfies struct sub) αs
satisfies struct@Structure{functions, relations} sub (Not φ) =
    not $ satisfies struct sub φ
satisfies struct@Structure{functions, relations} sub (Impl α β) = 
    satisfies struct sub α --> satisfies struct sub β
    where 
        (-->) True False = False
        (-->) _ _ = True
satisfies struct@Structure{functions, relations} sub (Forall x α) =
    and [satisfies struct (assignmentModifier sub x a) α | a <- enumerated]


