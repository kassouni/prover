module Formula where

import Numeric.Natural
import Prelude hiding (Functor(..))
import Data.Foldable ( Foldable(toList) )
import GHC.Exts (Constraint)
import Data.Functor

import Text.PrettyPrint
import BooleanAlgebra

data Term f v = Var v | Fun (f (Term f v))

data Formula r f v =
    Eq (Term f v) (Term f v) |
    Rel (r (Term f v)) |
    And [Formula r f v] |
    Not (Formula r f v) |
    Impl (Formula r f v) (Formula r f v) |
    Forall v (Formula r f v)

iff :: Formula r f v -> Formula r f v -> Formula r f v
iff α β = And [Impl β α, Impl α β]


varsT :: Foldable f => Term f v -> [v]
varsT (Var x) = [x]
varsT (Fun f) = concatMap varsT (toList f)

varsF ::Foldable f => Foldable r => Formula r f v -> [v]
varsF (Rel r) = concatMap varsT (toList r)
varsF _ = undefined

isAtomic :: Formula r f v -> Bool
isAtomic Eq {}  = True
isAtomic Rel {} = True
isAtomic _ = False

varFreeIn :: Foldable r => Eq v => Foldable f => Formula r f v -> v -> Bool
varFreeIn f@Eq {} x = x `elem` varsF f
varFreeIn f@Rel {} x = x `elem` varsF f
varFreeIn (And αs) x = any (`varFreeIn` x) αs
varFreeIn (Not φ) x = varFreeIn φ x
varFreeIn (Impl α β) x = varFreeIn α x || varFreeIn β x
varFreeIn (Forall y α) x = (y /= x) && varFreeIn α x

substitutableIn :: Foldable f => Eq v => Foldable r => Formula r f v -> Term f v -> v -> Bool
substitutableIn Rel {} t x = True
substitutableIn Eq {} t x = True
substitutableIn (And αs) t x = all (\α -> substitutableIn α t x) αs
substitutableIn (Not φ) t x = substitutableIn φ t x
substitutableIn (Impl α β) t x = substitutableIn α t x && substitutableIn β t x
substitutableIn φ@(Forall y α) t x = varFreeIn φ x || (y `notElem` varsT t && substitutableIn α t x)

substituteTerm :: Functor f => Eq v => v -> Term f v -> Term f v -> Term f v
substituteTerm x t (Fun f) = Fun $ substituteTerm x t <$> f
substituteTerm x t (Var y) = if y == x then t else Var y

substituteFormula :: Functor r => Eq v => Functor f =>  v -> Term f v -> Formula r f v -> Formula r f v
substituteFormula x t (Eq u1 u2) = Eq (substituteTerm x t u1) (substituteTerm x t u2)
substituteFormula x t (Rel r) = Rel (substituteTerm x t <$> r)
substituteFormula x t (And αs) = And (substituteFormula x t <$> αs)
substituteFormula x t (Not φ) = Not (substituteFormula x t φ)
substituteFormula x t (Impl α β) = Impl (substituteFormula x t α) (substituteFormula x t β)
substituteFormula x t φ@(Forall y α) = if x == y then φ else Forall y (substituteFormula x t α)
