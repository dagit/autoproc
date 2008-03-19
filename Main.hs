module Main where

import Autoproc.Run
import Autoproc.Rules.Dagit

main :: IO ()
main = autoprocMain $ dagitRules
