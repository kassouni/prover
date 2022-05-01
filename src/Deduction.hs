module Deduction where

import Formula
import BooleanAlgebra (implies, conjunction)
import NumberTheory
import Proposition (Proposition)
import Prelude hiding (sum)

type Axiom r f v = Formula r f v -> Bool

e1 :: Eq v => Axiom r f v
e1 (Eq (Var x) (Var y)) = x == y
e1 _ = False

unwrapEquality :: Formula r f v -> Maybe (Term f v, Term f v)
unwrapEquality (Eq t1 t2) = Just (t1, t2)
unwrapEquality _ = Nothing

unwrapAnds :: Formula r f v -> Maybe [Formula r f v]
unwrapAnds (And ands) = Just ands
unwrapAnds _ = Nothing

e2 :: Formula r f v -> Bool 
e2 (Impl conj eq) = do
    ands <- unwrapAnds

    return False

e3 = undefined