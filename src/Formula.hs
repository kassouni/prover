{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}


module Formula where

import Numeric.Natural
import Prelude hiding (Functor(..))
import Data.Foldable ( Foldable(toList) )
import GHC.Exts (Constraint)
import Data.Functor

import Text.PrettyPrint
import BooleanAlgebra

type Symbol = Char


data Term f where
    Var :: Symbol -> Term f
    Fun :: (Show (f (Term f)), Eq (f (Term f))) => f (Term f) -> Term f

instance Show (Term f) where
    show (Var s) = "Var " ++ show s
    show (Fun f) = "Fun (" ++ show f ++ ")"

instance Eq (Term f) where
    Var x == Var y = x == y
    Fun f1 == Fun f2 = f1 == f2
    _ == _ = False

data Formula r t where
    Eq :: (Show t, Eq t) => t -> t -> Formula r t
    Rel :: (Eq (r t), Show (r t)) => r t -> Formula r t
    Or :: Formula r t -> Formula r t -> Formula r t
    Not :: Formula r t -> Formula r t
    Forall :: Symbol -> Formula r t -> Formula r t

instance Eq (Formula r t) where
    Eq t1 t2 == Eq t3 t4 = t1 == t3 && t2 == t4
    Rel r1 == Rel r2 = r1 == r2
    Or f1 f2 == Or f3 f4 = f1 == f3 && f2 == f4
    Not f1 == Not f2 = f1 == f2
    Forall x1 f1 == Forall x2 f2 = x1 == x2 && f1 == f2
    _ == _ = False 

paren :: String -> String
paren s = "(" ++ s ++ ")"

instance Show (Formula r t) where
    show (Eq t1 t2) = "Eq " ++ "(" ++ show t1 ++ ")" ++  "(" ++ show t2 ++ ")"
    show (Rel r) = show r
    show (Or f1 f2) = "Or " ++ paren (show f1) ++ paren (show f2)
    show (Not f) = "Not " ++ paren (show f) 
    show (Forall x phi) = "Forall " ++ show x ++ " " ++ paren (show phi)

instance BooleanAlgebraic (Formula r (Term f))  where
    not = Not
    or = Or
    bottom = Not (Eq (Var 'x') (Var 'x'))

varsT :: Foldable f => Term f -> [Symbol]
varsT (Var x) = [x]
varsT (Fun f) = concatMap varsT (toList f)

varsF :: Foldable f => Foldable r => Formula r (Term f) -> [Symbol]
varsF (Rel r) = concatMap varsT (toList r)
varsF _ = undefined

isAtomic :: Formula r t -> Bool
isAtomic Eq {}  = True
isAtomic Rel {} = True
isAtomic _ = False

varFreeIn :: Foldable r => Foldable f => Formula r (Term f) -> Symbol -> Bool
varFreeIn f@Eq {} x = x `elem` varsF f
varFreeIn f@Rel {} x = x `elem` varsF f
varFreeIn (Or α β) x = varFreeIn α x || varFreeIn β x
varFreeIn (Not φ) x = varFreeIn φ x
varFreeIn (Forall y α) x = (y /= x) && varFreeIn α x

substitutableIn :: Foldable r => Foldable f => Formula r (Term f) -> Term f -> Symbol -> Bool
substitutableIn Rel {} t x = True
substitutableIn Eq {} t x = True
substitutableIn (Or α β) t x = substitutableIn α t x && substitutableIn β t x
substitutableIn (Not φ) t x = substitutableIn φ t x
substitutableIn φ@(Forall y α) t x = varFreeIn φ x || (y `notElem` varsT t && substitutableIn α t x)

substituteTerm :: Functor f => Symbol -> Term f -> Term f -> Term f
substituteTerm x t (Fun f) = Fun $ substituteTerm x t <$> f
substituteTerm x t (Var y) = if y == x then t else Var y

substituteFormula :: Functor r => Functor f => Symbol -> Term f -> Formula r (Term f) -> Formula r (Term f)
substituteFormula x t (Eq u1 u2) = Eq (substituteTerm x t u1) (substituteTerm x t u2)
substituteFormula x t (Rel r) = Rel (substituteTerm x t <$> r)
substituteFormula x t (Or α β) = Or (substituteFormula x t α) (substituteFormula x t β)
substituteFormula x t (Not φ) = Not (substituteFormula x t φ)
substituteFormula x t φ@(Forall y α) = if x == y then φ else Forall y (substituteFormula x t α)
