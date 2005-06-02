module Transform ( generate ) where

-- The purpose of this module is to define the transformations from
-- condition expression to procmail representation.

import qualified Procmail as Pm
import qualified Classifier as Cf
import List ( nub )

-- This raises the question, Why not use RecipeFlag for CExp?  The
-- reason is that we are trying to separate the final representation
-- (procmail) from the condition expression representation.  So in the
-- future if CExp flags change, we need only redefine this function.
-- Similar logic applies to Act and Cond.
-- Note: The above reasoning has saved me many times during development.
transformFlag :: Cf.Flag -> Pm.RecipeFlag
transformFlag Cf.Copy          = Pm.Copy
transformFlag Cf.Wait          = Pm.Wait
transformFlag Cf.IgnoreErrors  = Pm.IgnoreErrors
transformFlag (Cf.NeedLock b)  = (Pm.NeedLock b)
transformFlag Cf.Chain         = Pm.Chain
transformFlag Cf.CaseSensitive = Pm.CaseSensitive

transformCond :: Cf.Cond -> [Pm.Condition]
transformCond (Cf.Or c1 c2)      = error "transformCond cannot handle Or."
transformCond (Cf.And c1 c2)     = transformCond c1 ++ transformCond c2
transformCond (Cf.Not c)         = [Pm.Condition Pm.Invert c']
       where [Pm.Condition f c'] = transformCond c                            
transformCond Cf.Always          = [Pm.Condition Pm.Normal []]
transformCond (Cf.CheckHeader s) = [Pm.Condition Pm.Normal s]
transformCond (Cf.CheckBody s)   = [Pm.Condition Pm.Normal s]
transformCond (Cf.CheckMatch s)  = [Pm.Condition (Pm.Var "$MATCH") s]

transformAct :: Cf.Act -> Pm.Action
transformAct (Cf.File s)   = Pm.File s
transformAct (Cf.Filter s) = Pm.Pipe s
transformAct (Cf.Fwd es)   = Pm.Forward (map unAddress es)
             where unAddress (Cf.Addr a) = a
transformAct (Cf.Nest as)  = Pm.Nest (map transform as)

-- This pushes "not" as far down as possible.
-- This helps us to reach a "normal" form
distributeNot :: Cf.Cond -> Cf.Cond
distributeNot (Cf.Not (Cf.And c1 c2)) = Cf.Or (distributeNot (Cf.Not c1)) 
                                              (distributeNot (Cf.Not c2))
distributeNot (Cf.Not (Cf.Or c1 c2))  = Cf.And (distributeNot (Cf.Not c1)) 
                                               (distributeNot (Cf.Not c2))
distributeNot (Cf.Not (Cf.Not c))     = distributeNot c
distributeNot (Cf.And c1 c2)          = Cf.And (distributeNot c1)
                                               (distributeNot c2)
distributeNot (Cf.Or c1 c2)           = Cf.Or (distributeNot c1)
                                              (distributeNot c2)
distributeNot c                       = c

-- Each call to factor moves the Or one step closer to the top.  This must
-- be called many times by repeated to reach a normal form.
-- The goal here is to pull Or to the outside.
-- We don't worry about not, because that should have been handled by
-- distributeNot already.
factor :: Cf.Cond -> Cf.Cond
factor (Cf.And (Cf.Or c1 c2) c3) = (Cf.Or (Cf.And (factor c1) (factor c3))
                                          (Cf.And (factor c2) (factor c3)))
factor (Cf.And c1 (Cf.Or c2 c3)) = (Cf.Or (Cf.And (factor c1) (factor c2))
                                          (Cf.And (factor c1) (factor c3)))
factor (Cf.Or  c1 c2)            = (Cf.Or  (factor c1) (factor c2))
factor (Cf.And c1 c2)            = (Cf.And (factor c1) (factor c2))
factor c = c

repeated :: (Cf.Cond -> Cf.Cond) -> Cf.Cond -> Cf.Cond
repeated t c = loop c
         where loop c' = if c' == (t c') then c'
                         else repeated t (t c')

-- Procmail does not have a notion of Or, so we must put the
-- conditions at the same level and repeat the action.  This way, when
-- one of the conditions becomes true, the action is performed.
reduceOr :: Cf.CExp -> [Cf.CExp]
reduceOr (Cf.CExp fs (Cf.Or c1 c2) a)  = (reduceOr (Cf.CExp fs c1 a)) ++
                                         (reduceOr (Cf.CExp fs c2 a))
reduceOr x = [x]

-- 1. distrubuteNot
-- 2. factor
-- 3. reduceOr
-- The result is a list of CExp, none of which have an Or in their conditions
-- and not is only used on individual conditions
simplify :: Cf.CExp -> [Cf.CExp]
simplify (Cf.CExp fs c a) = reduceOr (Cf.CExp fs c'' a)
         where
         c'  = repeated distributeNot c
         c'' = repeated factor c'

-- This function assumses a simplified CExp, hence the first pattern match.
transform :: Cf.CExp -> Pm.PExp
transform (Cf.CExp _  (Cf.Or _ _) _) = error "use simplify."
transform (Cf.CExp fs c a)           = Pm.PExp (nub fs') (transformCond c)
                                                         (transformAct a)
    where
    fs'      = (if any then [Pm.CheckHeader,Pm.CheckBody] else
                 if body then [Pm.CheckBody] else
                   if header then [Pm.CheckHeader] else [])++newFlags
    body     = checksBody c
    header   = checksHeader c
    any      = checksAny c
    newFlags = (if isFilter a then [Pm.Wait, Pm.PipeAsFilter] else [])++
               (map transformFlag fs)

isFilter :: Cf.Act -> Bool    
isFilter (Cf.Filter _) = True
isFilter  _            = False

checksHeader :: Cf.Cond -> Bool
checksHeader (Cf.And c1 c2)       = checksHeader c1 || checksHeader c2
checksHeader (Cf.Or c1 c2)        = checksHeader c1 || checksHeader c2
checksHeader (Cf.Not c)           = checksHeader c
checksHeader (Cf.CheckHeader _)   = True
checksHeader _                    = False

checksBody :: Cf.Cond -> Bool
checksBody (Cf.And c1 c2)      = checksBody c1 || checksBody c2
checksBody (Cf.Or c1 c2)       = checksBody c1 || checksBody c2
checksBody (Cf.Not c)          = checksBody c
checksBody (Cf.CheckBody _)    = True
checksBody _                   = False

-- Perhaps checksEither is a better name?
checksAny :: Cf.Cond -> Bool
checksAny c = checksHeader c && checksBody c

-- This is how to generate a procmail recipe from a single expression
-- in the condition expression language.
generate :: Cf.CExp -> [Pm.PExp]
generate c = map transform (simplify c)
