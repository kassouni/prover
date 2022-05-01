module ZFC where
import Formula


data ZFCFun t = EmptySet
data ZFCRel t = Element t t


-- extensionality
a1 :: Formula ZFCRel ZFCFun Char
a1 = Forall 'x' (
        Forall 'y' ( 
            Impl
                (Forall 'z' (iff 
                    (Rel (Element (Var 'z') (Var 'x'))) 
                    (Rel (Element (Var 'z') (Var 'y'))))
                )
                (Eq (Var 'x') (Var 'y'))
        )
    )