module Exercise03Tests where

import Golf
import Test.HUnit

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel
        "test with empty list"
        ( TestCase
            ( assertEqual
                "should return bins only"
                "==========\n\
                \0123456789"
                (histogram [])
            )
        ),
      TestLabel
        "test with some numbers"
        ( TestCase
            ( assertEqual
                "should return histogram with 1 count in those numbers only"
                "   * *    \n\
                \==========\n\
                \0123456789"
                (histogram [3, 5])
            )
        ),
      TestLabel
        "test with all numbers"
        ( TestCase
            ( assertEqual
                "should return histogram with 1 count in all numbers"
                "**********\n\
                \==========\n\
                \0123456789"
                (histogram [0 .. 9])
            )
        ),
      TestLabel
        "test with many numbers"
        ( TestCase
            ( assertEqual
                "should return correct histogram"
                "    *     \n\
                \    *     \n\
                \    * *   \n\
                \ ******  *\n\
                \==========\n\
                \0123456789"
                (histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9])
            )
        )
    ]