module GodelSpec where

import Test.HUnit
import Formula
import NumberTheory
import Godel (godelNumber)


test :: Test
test = "0 = 0 encoding" ~: (((2^8) * (3^1025) * (5^1025)) ~?= godelNumber (Eq (Fun Zero) (Fun Zero)))

test2 :: Test
test2 = "1 /= 0 encoding" ~: 2^2 * 3 ^ (2^8 * 3^1025*5^(2^12 * 3^1025 + 1)) ~?= godelNumber (Not (Eq (Fun Zero) (Fun (S (Fun Zero)))))