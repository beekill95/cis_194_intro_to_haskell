module Main where
import ValidatingCreditCards
import Test.HUnit
import qualified System.Exit as Exit

testNegativeNumber :: Test
testNegativeNumber = TestCase (assertEqual "should return empty list" [] (toDigits (-17)))

tests :: Test
tests = TestList [TestLabel "testNegativeNumber" testNegativeNumber]

main :: IO()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
