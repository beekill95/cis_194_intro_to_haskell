module Exercise03 where

import Employee
import Party
import Test.HUnit

exercise03Tests :: Test
exercise03Tests =
  TestList
    [ TestLabel " test add an employee to an empty guest lists" $
        TestCase $
          assertEqual
            "should return a pair of guest lists where the employee is added to the left"
            (GL [Emp "Q" 1] 1, mempty)
            (nextLevel (Emp "Q" 1) []),
      TestLabel " test add an employee to a nonempty guest lists" $
        TestCase $
          assertEqual
            "should return a pair of guest lists where the employee is added to the list without direct subordinates"
            (GL [Emp "Q" 10, Emp "K" 5, Emp "I" 6] 21, GL [Emp "B" 1, Emp "E" 2] 3)
            ( nextLevel
                (Emp "Q" 10)
                [ (GL [Emp "B" 1] 1, GL [Emp "K" 5] 5),
                  (GL [Emp "E" 2] 2, GL [Emp "I" 6] 6)
                ]
            )
    ]
