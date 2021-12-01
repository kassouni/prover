module BooleanAlgebra where

import Prelude hiding (and, not, or)

class BooleanAlgebraic a where
    or :: a -> a -> a
    not :: a -> a
    bottom :: a

and :: BooleanAlgebraic a => a -> a -> a
and α β = not (or (not α) (not β))

implies :: BooleanAlgebraic a => a -> a -> a
implies α β = not (and α (not β))

top :: BooleanAlgebraic a => a
top = not bottom

conjunction :: BooleanAlgebraic a => [a] -> a
conjunction = foldl BooleanAlgebra.and top 

disjunction :: BooleanAlgebraic a => [a] -> a
disjunction = foldl or BooleanAlgebra.bottom