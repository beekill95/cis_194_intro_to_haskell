module Exercise05 where

import Calc
import StackVM
import Test.HUnit

exercise05Tests :: Test
exercise05Tests =
  TestList
    [ TestLabel "test with a literal" $
        TestCase $
          assertEqual
            "should return with one push statement"
            (Just [PushI 5])
            (compile "5"),
      TestLabel "test with simple addition" $
        TestCase $
          assertEqual
            "should return correct instructions order"
            (Just [PushI 5, PushI 3, Add])
            (compile "5 + 3"),
      TestLabel "test with simple multiplication" $
        TestCase $
          assertEqual
            "should return correct instructions order"
            (Just [PushI 5, PushI 3, Mul])
            (compile "5 * 3"),
      TestLabel "test with many additions" $
        TestCase $
          assertEqual
            "should return correct instructions order"
            (Just [PushI 5, PushI 3, PushI 5, PushI 9, Add, Add, Add])
            (compile "5 + 3 + 5 + 9"),
      TestLabel "test with many multiplications" $
        TestCase $
          assertEqual
            "should return correct instructions order"
            (Just [PushI 5, PushI 3, PushI 5, PushI 9, Mul, Mul, Mul])
            (compile "5 * 3 * 5 * 9"),
      TestLabel "test with many multiplications and additions" $
        TestCase $
          assertEqual
            "should return correct instructions order"
            (Just [PushI 5, PushI 3, PushI 5, PushI 9, Mul, Add, Add])
            (compile "5 + 3 + 5 * 9"),
      TestLabel "test with invalid expression" $
        TestCase $
          assertEqual
            "should return correct instructions order"
            Nothing
            (compile "5 + 3 + 5 *")
    ]