module Autoproc.Run where

import Autoproc.Classifier (CExp)
import Autoproc.Configuration (variables)
import Autoproc.Procmail (PExp, showLn)
import Autoproc.Transform (generate)

import Control.Monad.Writer (execWriter, Writer)

showVars :: [(String, String)] -> String
showVars []     = ""
showVars (v:vs) = (fst v) ++ " = " ++ (snd v) ++ "\n"
                  ++ showVars vs

showProcmailrc :: [(String, String)] -> [PExp] -> String
showProcmailrc vars ps = showVars (vars) ++
                          "\n\n" ++
                          "############################\n\n" ++
                          (ps >>= showLn)

autoprocMessage :: IO ()
autoprocMessage =  putStr $ unlines ["#.procmailrc",
                           "#  Automatically generated procmail recipes by Autoproc.",
                           "#  To find out more about Autoproc visit:",
                           "#    'http://www.codersbase.com/Autoproc'",
                           "#  To fetch the latest version of autoproc with Darcs:",
                           "#    'darcs get http://projects.codersbase.com/repos/autoproc'",
                           "",
                           ""]

autoprocMain :: Writer [CExp] a -> IO ()
autoprocMain rules = do autoprocMessage
                        putStrLn $ showProcmailrc variables $ concatMap generate $ execWriter rules
