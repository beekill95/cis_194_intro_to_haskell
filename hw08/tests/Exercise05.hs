module Exercise05 where

import Employee
import Party
import Test.HUnit

showGuestListTests :: Test
showGuestListTests =
  TestList
    [ TestLabel " test with empty guest list" $
        TestCase $
          assertEqual
            "should return correct format"
            ["Total fun: 0"]
            (showGuestList mempty),
      TestLabel " test with nonempty guest list" $
        TestCase $
          assertEqual
            "should return correct format and guests' names ordered"
            ["Total fun: 25", "Bob", "John", "Sarah", "Sue"]
            ( showGuestList
                ( GL
                    [Emp "Bob" 2, Emp "John" 1, Emp "Sue" 5, Emp "Sarah" 17]
                    25
                )
            )
    ]

exercise05Tests :: Test
exercise05Tests =
  TestList
    [ TestLabel " test `showGuestList`" showGuestListTests
    ]