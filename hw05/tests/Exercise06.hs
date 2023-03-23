module Exercise06 where

import Calc
import Test.HUnit

exercise06Tests :: Test
exercise06Tests =
  TestList
    [ TestLabel "test with expression with one variable" $
        TestCase $
          assertEqual
            "should return variable value"
            (Just 6)
            (withVars [("x", 6)] $ var "x"),
      TestLabel "test addition with one variable" $
        TestCase $
          assertEqual
            "should return correct addition result"
            (Just 9)
            (withVars [("x", 6)] $ add (var "x") (lit 3)),
      TestLabel "test mulitiplication with one variable" $
        TestCase $
          assertEqual
            "should return correct multiplication result"
            (Just 18)
            (withVars [("x", 6)] $ mul (var "x") (lit 3)),
      TestLabel "test mulitiplication with unknown variable" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (withVars [("y", 6)] $ mul (var "x") (lit 3)),
      TestLabel "test addition with unknown variable" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (withVars [("y", 6)] $ add (var "x") (lit 3)),
      TestLabel "test addition and multiplication with two variables" $
        TestCase $
          assertEqual
            "should return correct result"
            (Just 54)
            (withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x")))
    ]