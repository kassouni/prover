{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module NumberTheory where

import Formula
import Numeric.Natural
import Data.Foldable

import Structure

import Text.PrettyPrint.HughesPJClass

import Prelude hiding ((<>))

data NTFun term = 
    Zero | 
    S term | 
    Add term term | 
    Mult term term | 
    Exp term term 
        deriving (Show, Foldable, Functor, Eq)

type NTTerm = Term NTFun

zero :: Term NTFun
zero = Fun Zero

nBar :: Natural -> Term NTFun
nBar 0 = zero
nBar n = Fun (S (nBar (n - 1)))

example :: Term NTFun
example = Fun (Add (Fun (S zero)) (Var 'x'))

vs :: [Symbol]
vs = varsT example

data NTRel term = Lt term term deriving (Show, Foldable, Functor, Eq)

type NTFormula = Formula NTRel NTTerm

ppBinop :: Pretty a => a -> a -> Char -> Doc
ppBinop d1 d2 b = parens (pPrint d1) <+> char b <+> parens (pPrint d2)

instance Pretty NTTerm where
    pPrint (Var x) = char x 
    pPrint (Fun Zero) = text "0" 
    pPrint (Fun (S t)) = text "S" <> parens (pPrint t)
    pPrint (Fun (Add t1 t2)) = ppBinop t1 t2 '+'
    pPrint (Fun (Mult t1 t2)) = ppBinop t1 t2 '×'
    pPrint (Fun (Exp t1 t2)) = ppBinop t1 t2 '^'

instance Pretty t => Pretty (NTRel t) where
    pPrint (Lt t1 t2) = ppBinop t1 t2 '<'

instance Pretty NTFormula where
    pPrint (Eq t1 t2) = ppBinop t1 t2 '=' 
    pPrint (Rel r) = pPrint r
    pPrint (Or α β) = ppBinop α β '∨'
    pPrint (Not φ) = char '¬' <> parens (pPrint φ)
    pPrint (Forall x f) = parens (char '∀' <> char x) <> parens (pPrint f)

formula :: Formula NTRel NTTerm
formula = Forall 'x' (Rel (Lt (Fun Zero) (Fun Zero)))

ntFunInterp :: NTFun Natural -> Natural
ntFunInterp (S n) = n + 1
ntFunInterp Zero = 0
ntFunInterp (Add n1 n2) = n1 + n2
ntFunInterp (Mult n1 n2) = n1 * n2
ntFunInterp (Exp n1 n2) = n1 ^ n2

ntRelInterp :: NTRel Natural -> Bool
ntRelInterp (Lt n1 n2) = n1 < n2

numberTheory :: Structure Natural NTRel NTFun
numberTheory = Structure  {
    functions = ntFunInterp,
    relations = ntRelInterp 
}

instance Enumerable Natural where
    enumerated = [0..]

a1 :: NTFormula
-- (∀x) ¬Sx = 0
a1 = Forall 'x' (Not (Eq (nTsucc x) (Fun Zero)))

x = Var 'x'
y = Var 'y'

nTsucc :: Term NTFun -> Term NTFun
nTsucc = Fun . S

a2 :: NTFormula
-- (∀x) (∀y) [Sx = Sy -> x = y]
a2 = Forall 'x' (Forall 'y' (implies (Eq (nTsucc x) (nTsucc y)) (Eq x y)))

a3 :: NTFormula
-- (∀x) x + 0 = x
a3 = Forall 'x' (Eq (Fun (Add zero x)) x)

a4 :: NTFormula
-- (∀x) (∀y) x + Sy = S(x + y)
a4 = Forall 'x' (Forall 'y' (Eq (Fun $ Add x (nTsucc y)) (nTsucc (Fun $ Add x y))))

a5 :: NTFormula
-- (∀x) x * 0 = 0
a5 = Forall 'x' (Eq (Fun $ Mult x zero) zero)

a6 :: NTFormula
-- (∀x) (∀y) x + Sy = (x * y) + x
a6 = Forall 'x' (Forall 'y' (Eq (Fun (Mult x (nTsucc y))) (Fun (Add (Fun (Mult x y)) x))))

a7 :: NTFormula
-- (∀x) x E 0 = 0
a7 = Forall 'x' (Eq (Fun (Exp x zero)) (nTsucc zero))