module Exercise02Tests where

import Golf
import Test.HUnit

exercise02Tests :: Test
exercise02Tests =
  TestList
    [ TestLabel
        "test on empty list"
        ( TestCase
            ( assertEqual "should return empty list" [] (localMaxima [])
            )
        ),
      TestLabel
        "test on list with 2 local maximas"
        ( TestCase
            ( assertEqual
                "should return 2 maximas"
                [9, 6]
                (localMaxima [2, 9, 5, 6, 1])
            )
        ),
      TestLabel
        "test on list with 1 local maxima"
        ( TestCase
            ( assertEqual
                "should return 1 maxima"
                [4]
                (localMaxima [2, 3, 4, 1, 5])
            )
        ),
      TestLabel
        "test with increasing list"
        ( TestCase
            ( assertEqual
                "should return no maxima"
                []
                (localMaxima [1, 2, 3, 4, 5])
            )
        )
    ]
