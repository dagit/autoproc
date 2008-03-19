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

showProcmailrc :: [PExp] -> String
showProcmailrc ps = showVars (variables) ++ "\n\n" ++
                    "############################\n\n" ++
                    (ps >>= showLn)

autoprocMain :: Writer [CExp] a -> IO ()
autoprocMain rules = do putStr "#.procmailrc \
                              \ #  Automatically generated procmail recipes by Autoproc. \
                              \ #  To find out more about Autoproc visit: \
                              \ #    http://www.codersbase.com/Autoproc \
                              \ #  To fetch the latest version of autoproc with darcs: \
                              \ #    darcs get http://projects.codersbase.com/repos/autoproc \
                               \n\n"
                        putStrLn $ showProcmailrc $ concatMap generate $ execWriter rules
