module StructureSpec where

import NumberTheory
import Test.HUnit


import Structure
import Formula

testTwoXTwo :: Test
testTwoXTwo = "2 + 2 = 4 true in NT" ~: 
    let twoTimesTwoEqualsFour = Eq (Fun (Add (nBar 2) (nBar 2))) (nBar 4) in 
        assert $ satisfies numberTheory (const 0) twoTimesTwoEqualsFour



