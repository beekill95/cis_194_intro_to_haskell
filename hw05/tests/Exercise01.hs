module Exercise01 where

import Calc
import ExprT
import Test.HUnit

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel "Evaluate with literal" $
        TestCase $
          assertEqual
            "should return 1"
            1
            (eval $ Lit 1),
      TestLabel "Evaluate with addition" $
        TestCase $
          assertEqual
            "should return 5"
            5
            (eval $ Add (Lit 1) (Lit 4)),
      TestLabel "Evaluate with multiplication" $
        TestCase $
          assertEqual
            "should return 20"
            20
            (eval $ Mul (Lit 5) (Lit 4)),
      TestLabel "Evaluate with complex expression" $
        TestCase $
          assertEqual
            "should return 20"
            20
            (eval $ Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    ]
