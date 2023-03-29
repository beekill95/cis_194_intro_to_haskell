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
            (GL [Emp "U" 1] 1, mempty)
            (nextLevel (Emp "Q" 1) [])
    ]
