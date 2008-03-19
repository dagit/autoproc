module Main where

import Autoproc.Procmail (PExp, showLn)
import Autoproc.Transform (generate)
import Autoproc.Rules (rules)
import Autoproc.Configuration

import Control.Monad.Writer

showVars :: [(String, String)] -> String
showVars []     = ""
showVars (v:vs) = (fst v) ++ " = " ++ (snd v) ++ "\n"
                  ++ showVars vs

showProcmailrc :: [PExp] -> String
showProcmailrc ps = showVars (variables) ++ "\n\n" ++
                    "############################\n\n" ++
                    (ps >>= showLn)

main :: IO ()
main = do putStrLn "#.procmailrc"
          putStrLn "#  Automatically generated procmail recipes by Autoproc."
          putStrLn "#  To find out more about Autoproc visit:"
          putStrLn "#    http://www.codersbase.com/Autoproc"
          putStrLn "#  To fetch the latest version of autoproc with darcs:"
          putStrLn "#    darcs get http://projects.codersbase.com/repos/autoproc"
          putStr   "\n\n"
          putStrLn $ showProcmailrc $ concatMap generate $ execWriter rules

