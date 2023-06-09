module Exercise04 where

import Buffer
import JoinList
import Scrabble
import Sized
import Test.HUnit

-- Test `toString`
toStringTests :: Test
toStringTests =
  TestList
    [ TestLabel "test empty list" $
        TestCase $
          assertEqual
            "should return empty string"
            ""
            (toString (Empty :: JoinList (Score, Size) String)),
      TestLabel "test a single list" $
        TestCase $
          assertEqual
            "should return the string in the single list"
            "The Taiwanese girl asks for a knife."
            (toString (Single (Score 0, Size 1) "The Taiwanese girl asks for a knife.")),
      TestLabel "test an append list" $
        TestCase $
          assertEqual
            "should return the appended string in the two single lists"
            "The Taiwanese girl asked for a knife. And I told her that I don't have one."
            ( toString
                ( Append
                    (Score 0, Size 2)
                    (Single (Score 0, Size 1) "The Taiwanese girl asked for a knife. ")
                    (Single (Score 0, Size 1) "And I told her that I don't have one.")
                )
            ),
      TestLabel "test a nested append list" $
        TestCase $
          assertEqual
            "should return the appended string in the two single lists"
            "Khi hai ta ve mot nha. Khep doi mi chung mot giuong. Doi khi mo cung mot giac. Thuc giac chung mot gio."
            ( toString
                ( Append
                    (Score 0, Size 4)
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Khi hai ta ve mot nha. ")
                        (Single (Score 0, Size 1) "Khep doi mi chung mot giuong. ")
                    )
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Doi khi mo cung mot giac. ")
                        (Single (Score 0, Size 1) "Thuc giac chung mot gio.")
                    )
                )
            )
    ]

-- Test `fromString`
fromStringTests :: Test
fromStringTests =
  TestList
    [ TestLabel "test empty string" $
        TestCase $
          assertEqual
            "should return Empty"
            (Empty :: JoinList (Score, Size) String)
            (fromString ""),
      TestLabel "test string with one line" $
        TestCase $
          assertEqual
            "should return a Single list"
            (Single (Score 1, Size 1) "A")
            (fromString "A"),
      TestLabel "test string with multiple lines" $
        TestCase $
          assertEqual
            "should return a balance Append list"
            ( Append
                (Score 10, Size 5)
                ( Append
                    (Score 4, Size 2)
                    (Single (Score 1, Size 1) "A")
                    (Single (Score 3, Size 1) "B")
                )
                ( Append
                    (Score 6, Size 3)
                    (Single (Score 3, Size 1) "C")
                    ( Append
                        (Score 3, Size 2)
                        (Single (Score 2, Size 1) "D")
                        (Single (Score 1, Size 1) "E")
                    )
                )
            )
            (fromString "A\nB\nC\nD\nE")
    ]

-- Test `line`
lineTests :: Test
lineTests =
  TestList
    [ TestLabel "test empty list" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (line 5 (Empty :: JoinList (Score, Size) String)),
      TestLabel "test Single list with index 0" $
        TestCase $
          assertEqual
            "should return the string"
            (Just "Thuc giac chung mot gio.")
            (line 0 (Single (Score 0, Size 1) "Thuc giac chung mot gio.")),
      TestLabel "test Single list with invalid index" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            (line 5 (Single (Score 0, Size 1) "Thuc giac chung mot gio.")),
      TestLabel "test Append list with valid index" $
        TestCase $
          assertEqual
            "should return the string at index 2"
            (Just "Doi khi mo cung mot giac. ")
            ( line
                2
                ( Append
                    (Score 0, Size 4)
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Khi hai ta ve mot nha. ")
                        (Single (Score 0, Size 1) "Khep doi mi chung mot giuong. ")
                    )
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Doi khi mo cung mot giac. ")
                        (Single (Score 0, Size 1) "Thuc giac chung mot gio.")
                    )
                )
            ),
      TestLabel "test Append list with invalid index" $
        TestCase $
          assertEqual
            "should return Nothing"
            Nothing
            ( line
                4
                ( Append
                    (Score 0, Size 4)
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Khi hai ta ve mot nha. ")
                        (Single (Score 0, Size 1) "Khep doi mi chung mot giuong. ")
                    )
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Doi khi mo cung mot giac. ")
                        (Single (Score 0, Size 1) "Thuc giac chung mot gio.")
                    )
                )
            )
    ]

-- Test `replaceLine`
replaceLineTests :: Test
replaceLineTests =
  TestList
    [ TestLabel "test replace empty list with valid index" $
        TestCase $
          assertEqual
            "should return Empty list"
            (Empty :: (JoinList (Score, Size) String))
            (replaceLine 0 "A" Empty),
      TestLabel "test replace empty list with invalid index" $
        TestCase $
          assertEqual
            "should return an empty list"
            Empty
            (replaceLine 5 "A" (Empty :: (JoinList (Score, Size) String))),
      TestLabel "test replace single list with negative index" $
        TestCase $
          assertEqual
            "should return the list unchanged"
            (Single (Score 1, Size 1) "A")
            (replaceLine (-5) "Q" (Single (Score 1, Size 1) "A")),
      TestLabel "test replace single list with valid index" $
        TestCase $
          assertEqual
            "should return the list with score and content changed"
            (Single (Score 10, Size 1) "Q")
            (replaceLine 0 "Q" (Single (Score 1, Size 1) "A")),
      TestLabel "test replace single list with invalid index" $
        TestCase $
          assertEqual
            "should return the list unchanged"
            (Single (Score 1, Size 1) "A")
            (replaceLine 5 "Q" (Single (Score 1, Size 1) "A")),
      TestLabel "test replace an append list with a negative index" $
        TestCase $
          assertEqual
            "should return the list unchanged"
            ( Append
                (Score 13, Size 4)
                ( Append
                    (Score 11, Size 2)
                    (Single (Score 10, Size 1) "Q")
                    (Single (Score 1, Size 1) "u")
                )
                ( Append
                    (Score 2, Size 2)
                    (Single (Score 1, Size 1) "a")
                    (Single (Score 1, Size 1) "n")
                )
            )
            ( replaceLine
                (-1)
                "C"
                ( Append
                    (Score 13, Size 4)
                    ( Append
                        (Score 11, Size 2)
                        (Single (Score 10, Size 1) "Q")
                        (Single (Score 1, Size 1) "u")
                    )
                    ( Append
                        (Score 2, Size 2)
                        (Single (Score 1, Size 1) "a")
                        (Single (Score 1, Size 1) "n")
                    )
                )
            ),
      TestLabel "test replace an append list with an invalid index" $
        TestCase $
          assertEqual
            "should return the list unchanged"
            ( Append
                (Score 13, Size 4)
                ( Append
                    (Score 11, Size 2)
                    (Single (Score 10, Size 1) "Q")
                    (Single (Score 1, Size 1) "u")
                )
                ( Append
                    (Score 2, Size 2)
                    (Single (Score 1, Size 1) "a")
                    (Single (Score 1, Size 1) "n")
                )
            )
            ( replaceLine
                5
                "C"
                ( Append
                    (Score 13, Size 4)
                    ( Append
                        (Score 11, Size 2)
                        (Single (Score 10, Size 1) "Q")
                        (Single (Score 1, Size 1) "u")
                    )
                    ( Append
                        (Score 2, Size 2)
                        (Single (Score 1, Size 1) "a")
                        (Single (Score 1, Size 1) "n")
                    )
                )
            ),
      TestLabel "test replace an append list with a valid index on the first half" $
        TestCase $
          assertEqual
            "should return the list with score and content changed"
            ( Append
                (Score 15, Size 4)
                ( Append
                    (Score 13, Size 2)
                    (Single (Score 10, Size 1) "Q")
                    (Single (Score 3, Size 1) "C")
                )
                ( Append
                    (Score 2, Size 2)
                    (Single (Score 1, Size 1) "a")
                    (Single (Score 1, Size 1) "n")
                )
            )
            ( replaceLine
                1
                "C"
                ( Append
                    (Score 13, Size 4)
                    ( Append
                        (Score 11, Size 2)
                        (Single (Score 10, Size 1) "Q")
                        (Single (Score 1, Size 1) "u")
                    )
                    ( Append
                        (Score 2, Size 2)
                        (Single (Score 1, Size 1) "a")
                        (Single (Score 1, Size 1) "n")
                    )
                )
            ),
      TestLabel "test replace an append list with a valid index on the second half" $
        TestCase $
          assertEqual
            "should return the list with score and content changed"
            ( Append
                (Score 15, Size 4)
                ( Append
                    (Score 11, Size 2)
                    (Single (Score 10, Size 1) "Q")
                    (Single (Score 1, Size 1) "u")
                )
                ( Append
                    (Score 4, Size 2)
                    (Single (Score 1, Size 1) "a")
                    (Single (Score 3, Size 1) "C")
                )
            )
            ( replaceLine
                3
                "C"
                ( Append
                    (Score 13, Size 4)
                    ( Append
                        (Score 11, Size 2)
                        (Single (Score 10, Size 1) "Q")
                        (Single (Score 1, Size 1) "u")
                    )
                    ( Append
                        (Score 2, Size 2)
                        (Single (Score 1, Size 1) "a")
                        (Single (Score 1, Size 1) "n")
                    )
                )
            )
    ]

-- Test `numLines`
numLinesTests :: Test
numLinesTests =
  TestList
    [ TestLabel "test empty list" $
        TestCase $
          assertEqual
            "should return 0"
            0
            (numLines (Empty :: JoinList (Score, Size) String)),
      TestLabel "test Single list" $
        TestCase $
          assertEqual
            "should return 1"
            1
            (numLines (Single (Score 0, Size 1) "Thuc giac chung mot gio.")),
      TestLabel "test append list" $
        TestCase $
          assertEqual
            "should return correct number of lines"
            4
            ( numLines
                ( Append
                    (Score 0, Size 4)
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Khi hai ta ve mot nha. ")
                        (Single (Score 0, Size 1) "Khep doi mi chung mot giuong. ")
                    )
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Doi khi mo cung mot giac. ")
                        (Single (Score 0, Size 1) "Thuc giac chung mot gio.")
                    )
                )
            )
    ]

-- Test `value`
valueTests :: Test
valueTests =
  TestList
    [ TestLabel "test empty list" $
        TestCase $
          assertEqual
            "should return 0"
            0
            (value (Empty :: JoinList (Score, Size) String)),
      TestLabel "test Single list" $
        TestCase $
          assertEqual
            "should return correct score"
            500
            (value (Single (Score 500, Size 1) "Thuc giac chung mot gio.")),
      TestLabel "test append list" $
        TestCase $
          assertEqual
            "should return correct score"
            270
            ( value
                ( Append
                    (Score 270, Size 4)
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Khi hai ta ve mot nha. ")
                        (Single (Score 0, Size 1) "Khep doi mi chung mot giuong. ")
                    )
                    ( Append
                        (Score 0, Size 2)
                        (Single (Score 0, Size 1) "Doi khi mo cung mot giac. ")
                        (Single (Score 0, Size 1) "Thuc giac chung mot gio.")
                    )
                )
            )
    ]

-- Collect all tests.
exercise04Tests :: Test
exercise04Tests =
  TestList
    [ TestLabel "`toString` tests" toStringTests,
      TestLabel "`fromString` tests" fromStringTests,
      TestLabel "`line` tests" lineTests,
      TestLabel "`replaceLine` tests" replaceLineTests,
      TestLabel "`replaceLine` tests" numLinesTests,
      TestLabel "`value` tests" valueTests
    ]