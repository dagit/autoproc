module Classifier where

-- The purpose of this module is to define the abstract and concrete
-- syntax for the condition expression language.

import Monad hiding (when)
-- Some functions in this module get their meaning and values from
-- Configuration module.  If you want to change a default such as
-- locking, check the Configuration module.
import Configuration

data EmailAddress = Addr String deriving Show

data Mailbox = Mailbox String

data CExp = CExp [Flag] Cond Act deriving Show

data Flag = Copy
     | Wait
     | IgnoreErrors
     | RawWrite
     | NeedLock Bool
     | Chain
     | CaseSensitive deriving (Eq, Show)

data Cond = And Cond Cond
     | Or Cond Cond
     | Not Cond
     | Always
     | CheckMatch String
     | CheckHeader String
     | CheckBody String deriving Show

data Act = File String
     | Fwd [EmailAddress]
     | Filter String
     | Nest [CExp]  deriving Show

type Rule = Cond -> Act -> CExp

---------------------------------------------------------------------------
-- Basic functions for manipulating conditions and creating Rules

(.&&.) :: Cond -> Cond -> Cond
c1 .&&. c2 = And c1 c2

(.||.) :: Cond -> Cond -> Cond
c1 .||. c2 = Or c1 c2

subject, body, said :: String -> Cond
subject s = CheckHeader ("^Subject.*"++s)
body s    = CheckBody s
said s    = subject s .||. body s

from, to, to_ :: EmailAddress -> Cond
from (Addr s) = CheckHeader ("^From.*"++s)
to   (Addr s) = CheckHeader ("^TO"++s)
to_  (Addr s) = CheckHeader ("^TO_"++s)

when :: Rule
when c a = whenWithOptions [lock] c a

whenWithOptions :: [Flag] -> Rule
whenWithOptions fs c a = CExp fs c a

placeIn :: Mailbox -> Act
placeIn (Mailbox m) = File m

also :: Act -> Act -> Act
also (Nest as) (Nest bs) = Nest (flagAllButLast Copy (as++bs))
also (Nest as) b         = Nest (flagAllButLast Copy 
                                (as++[whenWithOptions [] Always b]))
also a         (Nest bs) = Nest (flagAllButLast Copy 
                                ((whenWithOptions [] Always a):bs))
also a         b         = Nest (flagAllButLast Copy 
                                [(whenWithOptions [] Always a), 
                                 (whenWithOptions [] Always b)])

flagAllButLast :: Flag -> [CExp] -> [CExp]
flagAllButLast f [] = []
flagAllButLast f cs = (map (addFlag f) (init cs))++[removeFlag f (last cs)]

addFlag :: Flag -> CExp -> CExp
addFlag f (CExp fs a c) = (CExp (f:fs) a c)

removeFlag :: Flag -> CExp -> CExp
removeFlag f (CExp fs a c) = (CExp (filter (/= f) fs) a c)

forwardTo :: [EmailAddress] -> Act
forwardTo es = Fwd es

isSpam :: Cond
isSpam = CheckHeader ("^x-spam-status: yes") .||.
         CheckHeader ("^x-spam-flag: yes")

spamLevel :: Int -> Cond
spamLevel n = CheckHeader ("^x-spam-Level: "++replicate n '*')

--------------------------------------------------------------------------
-- Match monad is just the identity monad, this makes it so that the user
-- cannot use match arbitrarily.  Used a monad instead of just a data
-- wrapper because now we can use the monad utilities like liftM

data Match a = Match a

instance Monad Match where
         return = Match
         (>>=) (Match a) f = (f a)

match :: Match String
match = return "$MATCH"

whenMatch :: Match Cond -> Match Act -> CExp
whenMatch mc ma = whenMatchWithOptions [lock] mc ma

whenMatchWithOptions :: [Flag] -> Match Cond -> Match Act -> CExp
whenMatchWithOptions fs (Match c) (Match a) = CExp fs c a

placeInUsingMatch :: Match Mailbox -> Match Act
placeInUsingMatch = liftM placeIn

(%) :: Cond -> String -> Match Cond
(CheckHeader s1) % s2 = return (CheckHeader (s1++"\\/"++s2))
(CheckBody   s1) % s2 = return (CheckBody   (s1++"\\/"++s2))
(CheckMatch  s1) % s2 = return (CheckMatch  (s1++"\\/"++s2))

refineBy :: Match Cond -> Match Cond -> Match Cond
refineBy = liftM2 (.&&.)

alsoUsingMatch :: Match Act -> Match Act -> Match Act
alsoUsingMatch = liftM2 also

---------------------------------------------------------------------------
-- A few functions to create short hand for sorting
sortBy :: (a -> Cond) -> a -> Mailbox -> CExp
sortBy f s m = when (f s) (placeIn m)

sortByTo_, sortByTo, sortByFrom :: EmailAddress -> Mailbox -> CExp
sortByTo_     = sortBy to_
sortByTo      = sortBy to
sortByFrom    = sortBy from

sortBySubject :: String -> Mailbox -> CExp
sortBySubject = sortBy subject

----------------------------------------------------------------------------
-- Everything below here depends on the values in the Configuration module

simpleSortByFrom, simpleSortByTo_, simpleSortByTo :: String -> CExp
simpleSortByFrom s = sortByFrom (Addr s) (mailbox s)
simpleSortByTo   s = sortByTo   (Addr s) (mailbox s)
simpleSortByTo_  s = sortByTo_  (Addr s) (mailbox s)

mailbox :: String -> Mailbox
mailbox s = Mailbox (boxPrefix++s)

mailBoxFromMatch :: Match String -> Match Mailbox
mailBoxFromMatch = liftM mailbox

lock :: Flag
lock = NeedLock lockDefault

---------------------------------------------------------------------------
-- This is the actually "Classifier" implementation.  It's not as powerful.
-- Please consider this "syntax" to be experimental.

type Class = (String, [Cond])

type Trigger = (String, Int, Act)

type Classifier = CExp

mkTrigger :: Trigger -> Classifier
mkTrigger (s, i, a) = when (CheckHeader 
                            ("^"++(mkHeader s)++(replicate i '*')))
                       a

mkClassifiers :: Class -> [Classifier]
mkClassifiers (s, cs) = more (length cs) s cs
              where
              more n s []     = []
              more n s (x:xs) = when x (Nest (incrementHeader s n)) : 
                                (more n s xs)

incrementHeader :: String -> Int -> [CExp]
incrementHeader s n = [whenMatch ((CheckHeader ("^"++mkHeader s)) % 
                                 (replicate n '*'))
                       updateHeader,
                      when (Not (CheckHeader ("^"++mkHeader s)))
                      writeHeader]   
  where 
  updateHeader = do { m <- match; 
                      return (Filter ("formail -I\""++mkHeader s++m++"*\"")) }
  writeHeader  = Filter ("formail -I\""++mkHeader s++"*\"")

mkHeader :: String -> String
mkHeader s = "X-classifier-"++s++": "

classify :: [Class] -> [Trigger] -> [CExp]
classify cs ts = (cs >>= mkClassifiers) ++ (map mkTrigger ts)

classifyBy :: (String, Cond) -> Act -> [CExp]
classifyBy (s, c) a = classify [(s,[c])] [(s, 1, a)]

classifyByAddress::(EmailAddress -> Cond) -> EmailAddress -> Mailbox -> [CExp]
classifyByAddress f e@(Addr s) m = classify [(s, [f e])] [(s, 1, placeIn m)]

classifyByTo_, classifyByTo, classifyByFrom:: EmailAddress -> Mailbox -> [CExp]
classifyByTo_  = classifyByAddress to_  
classifyByTo   = classifyByAddress to   
classifyByFrom = classifyByAddress from 

classifyBySubject :: String -> Mailbox -> [CExp]
classifyBySubject s m = classify [(s, [subject s])] [(s, 1, placeIn m)]

simpleClassifyByFrom, simpleClassifyByTo_, simpleClassifyByTo::String -> [CExp]
simpleClassifyByFrom s = classifyByFrom (Addr s) (mailbox s)
simpleClassifyByTo   s = classifyByTo   (Addr s) (mailbox s)
simpleClassifyByTo_  s = classifyByTo_  (Addr s) (mailbox s)
