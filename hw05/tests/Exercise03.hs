module Exercise03 where

import Calc
import ExprT
import Test.HUnit

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel "test with literal" $
        TestCase $
          assertEqual "should return literal" (Lit 5) (reify $ lit 5),
      TestLabel "test with addition" $
        TestCase $
          assertEqual
            "should return addition epxression"
            (Add (Lit 5) (Lit 2))
            (reify $ add (lit 5) (lit 2)),
      TestLabel "test with multiplication" $
        TestCase $
          assertEqual
            "should return multiplication epxression"
            (Mul (Lit 5) (Lit 2))
            (reify $ mul (lit 5) (lit 2)),
      TestLabel "test with complex expression" $
        TestCase $
          assertEqual
            "should return correct epxression"
            (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
            (reify $ mul (add (lit 2) (lit 3)) (lit 4))
    ]