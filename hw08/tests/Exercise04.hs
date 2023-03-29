module Exercise04 where

import Employee
import Party
import Test.HUnit

exercise04Tests :: Test
exercise04Tests =
  TestList
    [ TestLabel " test company" $
        TestCase $
          assertEqual
            "should return the correct guest list"
            (GL [Emp "Bob" 2, Emp "John" 1, Emp "Sue" 5, Emp "Sarah" 17] 25)
            (maxFun testCompany),
      TestLabel " test company2" $
        TestCase $
          assertEqual
            "should return the correct guest list"
            (GL [Emp "Bob" 3, Emp "John" 1, Emp "Sue" 5, Emp "Sarah" 17] 26)
            (maxFun testCompany2)
    ]
