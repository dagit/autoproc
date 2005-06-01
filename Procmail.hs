module Procmail where
-- One thing to keep in mind:
-- Procmail seems to "just work" by running regular expressions over the
-- email and doing something.

-- The goal of this module is to capture the abstract syntax of procmail
-- and to output to the procmail syntax.

import List ( sort )

data PExp = PExp [RecipeFlag] [Condition] Action

data RecipeFlag = CheckHeader | CheckBody 
     | CaseSensitive | Chain
     | ElseIf | PipeAsFilter | Copy 
     | Wait | IgnoreErrors | RawWrite | NeedLock Bool deriving (Eq, Ord)

data Condition = Condition ConditionFlag String
data ConditionFlag = Normal | Invert | Eval | UseExitCode 
     | LessThan | GreaterThan | Var String
     
{- 

 There are two types of actions:

 1) delivering
 2) non-delivering

 The difference is that a delivering message stops execution of rules
 a non-delivering rule feeds a carbon-copy of the message to rule and
 then continues on trying rules.

-}

data Action = Forward [String] | Pipe String | File String
     | Nest [PExp]

--This is useful when used with the list monad
--to print each element of the list xs on a line by itself use:
-- xs >>= showLn
showLn :: (Show a) => a -> String
showLn = (++ "\n") . show

instance Show PExp where
         show (PExp fs cs a) = ":0"++showFlags fs++"\n"
                               ++(cs >>= (\x -> if show x == "" then "" 
                                                else showLn x))
                               ++show a++"\n"
              where
              showFlags [NeedLock b] = show (NeedLock b)
              showFlags fs         = " "++((show =<<) . sort) fs

instance Show RecipeFlag where
         show CheckHeader      = "H"
         show CheckBody        = "B"
         show CaseSensitive    = "D"
         show Chain            = "A"
         show ElseIf           = "E"
         show PipeAsFilter     = "f"
         show Copy             = "c"
         show Wait             = "w"
         show IgnoreErrors     = "i"
         show RawWrite         = "r"
         show (NeedLock True)  = ":"
         show (NeedLock False) = ""

instance Show Condition where
         show (Condition cf []) = ""
         show (Condition cf s)  = "* "++show cf++s

instance Show ConditionFlag where
         show Normal      = ""
         show Invert      = "!"
         show Eval        = "$"
         show UseExitCode = "?"
         show LessThan    = "<"
         show GreaterThan = ">"
         show (Var s)     = s++" ?? "

instance Show Action where
         show (Nest es)    = "{ \n"++(es >>= showLn)++"}"
         show (File s)     = s
         show (Forward es) = "! "++(es >>= (++ " "))
         show (Pipe s)     = "| "++s


{-
Here is a test case to try:
PExp [] [(Condition Normal "^From.*peter"), (Condition Normal "^Subject:.*compilers")] (Nest [PExp [Copy] [] (Forward "william@somewhere.edu"), PExp [] [] (File "petcompil")])

It should generate output equivalent to:
:0
* ^From.*peter
* ^Subject:.*compilers
  {
    :0 c
    ! william@somewhere.edu

    :0
    petcompil
  }

-}
