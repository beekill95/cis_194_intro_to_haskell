module Main where

import Log
import LogAnalysis

main :: IO ()
main = testWhatWentWrong parse whatWentWrong "data/error.log" >>= print