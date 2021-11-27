import Test.HUnit
import Formula
import StringTheory



main :: IO ()
main = do
    runTestTT $ TestList [test1]
    return ()

test1 :: Test
test1 =  "termSubst" ~:
    let g = function "g" in
    let h = function "h" in
    let q = relation "Q" in
    let r = relation "R" in
    let p = relation "P" in
    let x = Var 'x' in
    let y = Var 'y' in
    let z = Var 'z' in
    let c = Var 'c' in
    let φ = implies (p [x, y]) (Or (Forall 'x' (q [g [x], z])) (Forall 'y' (r [x, h [x]]))) in
    let φ' = implies (p [g [c], y]) (Or (Forall 'x' (q [g [x], z])) (Forall 'y' (r [g [c], h [g [c]]]))) in
    substituteFormula 'x' (Fun (F "g" [Var 'c'])) φ ~?= φ'


