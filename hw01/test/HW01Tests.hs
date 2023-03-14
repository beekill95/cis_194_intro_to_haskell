module Main where
import BasicSum
import Test.HUnit
import qualified System.Exit as Exit

main :: IO()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
