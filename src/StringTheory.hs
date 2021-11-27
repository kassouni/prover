{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module StringTheory where

import Formula

data SFun term = F String [term] deriving (Foldable, Functor, Eq, Show)

type STerm = Term SFun

data SRel term = R String [term] deriving (Eq, Functor, Foldable, Show)

type SFormula = Formula SRel STerm

function :: String -> [Term SFun] -> Term SFun
function str ts = Fun (F str ts)

relation :: String -> [Term SFun] -> Formula SRel (Term SFun)
relation str ts = Rel (R str ts)

