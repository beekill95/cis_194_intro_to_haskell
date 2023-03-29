module Exercise01 where

import Employee
import Party
import Test.HUnit

exercise011Tests :: Test
exercise011Tests =
  TestList
    [ TestLabel " add an employee to an empty guest list" $
        TestCase $
          assertEqual
            "should return a new guest list with the employee added"
            (GL [Emp "Q" 1] 1)
            (glCons (Emp "Q" 1) mempty),
      TestLabel " add an employee to a nonempty guest list" $
        TestCase $
          assertEqual
            "should return a new guest list with the employeed added and fun score updated"
            (GL [Emp "Y" 19, Emp "Q" 1] 20)
            (glCons (Emp "Y" 19) (GL [Emp "Q" 1] 1))
    ]

exercise012Tests :: Test
exercise012Tests =
  TestList
    [ TestLabel " test `mempty` to be an empty guest list" $
        TestCase $
          assertEqual
            "should return an empty guest list"
            (GL [] 0)
            mempty,
      TestLabel " test `<>` to concatenate two guest lists" $
        TestCase $
          assertEqual
            "should return a concatenated guest list"
            (GL [Emp "Y" 19, Emp "Q" 1] 20)
            (GL [Emp "Y" 19] 19 <> GL [Emp "Q" 1] 1)
    ]

exercise013Tests :: Test
exercise013Tests =
  TestList
    [ TestLabel " test more fun guest list on the left" $
        TestCase $
          assertEqual
            "should return the guest list on the left"
            (GL [Emp "Y" 19] 19)
            (moreFun (GL [Emp "Y" 19] 19) (GL [Emp "Q" 1] 1)),
      TestLabel " test more fun guest list on the right" $
        TestCase $
          assertEqual
            "should return the guest list on the right"
            (GL [Emp "Y" 19] 19)
            (moreFun (GL [Emp "Q" 1] 1) (GL [Emp "Y" 19] 19)),
      TestLabel " test both guest lists with equal fun" $
        TestCase $
          assertEqual
            "should return the guest list on the left"
            (GL [Emp "Q" 1] 1)
            (moreFun (GL [Emp "Q" 1] 1) (GL [Emp "L" 1] 1))
    ]

exercise01Tests :: Test
exercise01Tests =
  TestList
    [ TestLabel " exercise 1.1 " exercise011Tests,
      TestLabel " exercise 1.2 " exercise012Tests,
      TestLabel " exercise 1.3 " exercise013Tests
    ]
