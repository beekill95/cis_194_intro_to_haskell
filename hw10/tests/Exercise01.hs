module Exercise01 where

import AParser
import Data.Char
import Test.HUnit

upperCharParser = satisfy isUpper

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel "+> Identity Test 1 <+" $
        TestCase $
          assertEqual
            "should return the same result"
            (runParser (fmap id upperCharParser) "ABC")
            (id $ runParser upperCharParser "ABC"),
      TestLabel "+> Identity Test 2 <+" $
        TestCase $
          assertEqual
            "should return the same result"
            (runParser (fmap id upperCharParser) "1BC")
            (id $ runParser upperCharParser "1BC"),
      TestLabel "+> Composition Test <+" $
        TestCase $
          assertEqual
            "should return the same result"
            (runParser (fmap ((+ 3) . (* 5)) posInt) "31")
            (runParser (fmap (+ 3) $ fmap (* 5) posInt) "31")
    ]