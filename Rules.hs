module Rules where 

import Classifier 
import Transform 
import Monad hiding (when)
import Control.Monad.Writer hiding (when)
import System

-- Any rules that you create need to end up in the rules list.  Other
-- than that, feel free to define your own rules using these rules an
-- examples.

-- A rule is something of the form:
-- when condition action
-- Examples of condition include, (from (Addr "foo@bar")),
-- (subject "Hi again").  And example actions are things like:
-- (placIn (mailbox "steve")), (forward [Addr "friend@yahoo.com"])

-- I have created some "aliases" for commonly used constructions
-- For example, simpleSortByFrom "joe", is equivalent to:
-- when (from (Addr "joe")) (placeIn (mailbox "joe"))  
-- For a full list of what is possible, check the Classifier module.

--Rules start here:
rules = do spamc; spamcheck; sarah; mom; dad; rogan; lkm; cvsupdates;
           cdspaper; bugs; forms3Tech; forms3; euses; darcsUsers;
           darcsDevel; sbclDevel; ogi; clispDevel; csGradTalk;
           classes; nwn; debian; csmaillist; momentum; fixReplyTo; defaultRule
           

-- default action
-- I use this rule to make sure any mail that is not sorted goes into
-- my mail spool.
defaultRule = when Always (File "/var/mail/dagit")

--Friends/Family
-- If the eamil address contains "sparish", then place the email into a folder
-- by the name "sparish".  Actually, the name of the mailbox is created by
-- appending boxPrefix which is defined in the Configuration module.
sarah = simpleSortByFrom "sparish"
-- Similar to above, except we are sorting based on the subject line,
-- and giving the mailbox.  As above, boxPrefix will be added to "nwn".
nwn   = sortBySubject "nwn" (mailbox "nwn")
-- If the email address contains Griffinmndm then the email is from Mom.
mom   = sortByFrom (Addr "Griffinmndm") (mailbox "mom")
dad   = sortByFrom (Addr "naturesgifts") (mailbox "dad")
rogan = sortByFrom (Addr "creswick") (mailbox "rogan")

--Mailling lists
lkm = sortByTo_ (Addr "linux-kernel@vger.kernel.org") (mailbox "linux-kernel")

-- This is an example of the general syntax.  The above examples are converted
-- to an analogous when statment.
cvsupdates = when (subject "\\[forms3-tech\\]" .&&. subject "\\[cvs\\]") 
             (placeIn (mailbox "cvsupdates"))

cdspaper = when (subject "\\[CDs Paper Update\\]") (placeIn (mailbox "cdpaper"))

bugs = when (subject "\\[forms3-tech\\]" .&&. subject "\\[jira\\]") 
       (placeIn (mailbox "bugs"))

forms3Tech = simpleSortByTo_ "forms3-tech"

forms3 = simpleSortByTo_ "forms3"

euses = when ((subject "\\[eusesnewsletter\\]") .||. 
              (to_ (Addr "eusesosugrads"))  .||. 
              (to_ (Addr "eusesall")))
        (placeIn (mailbox "euses"))

darcsUsers = simpleSortByTo_ "darcs-users"

darcsDevel = simpleSortByTo_ "darcs-devel"

sbclDevel  = simpleSortByTo_ "sbcl-devel"

ogi        = when (subject "OGI") (placeIn (mailbox "csmaillist"))

clispDevel = simpleSortByTo_ "clisp-devel"

csGradTalk = simpleSortByTo_ "cs-grad-talk"

-- This rule has a custom header check.  It checks the header of the 
-- email for a line that begins with "X-Loop: ...". 
-- People familiar with regular expressions will recognize the meaning.
-- of ^ and .*
debian     = when (CheckHeader "^X-Loop: debian.*@lists.debian.org") 
             (placeIn (mailbox "debian"))

csmaillist = when (subject "\\[cs-grads\\]"   .||. 
                   subject "\\[eecs-grads\\]" .||.
                   to_ (Addr "eecs-grads"))
             (placeIn (mailbox "csmaillist"))

--Class lists

-- This is a rather sophisticated example demonstrating matching.
-- When the % operator is used, the text that matches the regular
-- expression on the right hand side of the %, is stored in the variable
-- match.  This requires whenMatch instead of when.
classes    = whenMatch (((to_ (Addr "class-")) % ".*@") `refineBy` 
                        ((CheckMatch "()") % "[^@]+"))
             (placeInUsingMatch (mailBoxFromMatch match))

-- Example showing that usage of match is checked using the type
-- system.  In this example match would not have a value because the
-- operator % has not been used in the condition.
--test = when (to_ (Addr "test"))
--      (placeInUsingMatch (mailbox match))

--spam rules
-- A filter is a special action that transforms the email for 
-- the benefit of future rules.  This particular rule,
-- hands the email off to spam assassin so that it can be checked for
-- signs of spam.
spamc     = when Always (Filter "/usr/local/bin/spamc")

-- isSpam and spamLevel are special conditions for use with spamassassin.
spamcheck = when (isSpam       .||.
                 (spamLevel 3) .||.
                 (from (Addr "nationalmkt@planters.net")))
            (placeIn (mailbox "caughtspam"))

momentum  = sortBySubject "momentum!" (mailbox "caughtspam")

--Random Examples
-- An example that demonstrates forwarding an email.
sharing   = when (said "caring" .&&. from (Addr "ecards"))
            (forwardTo [(Addr "dagit@codersbase.com"), 
                      (Addr "thedagit@hotmail.com")])

-- This rules "fixes" the reply-to header of a mailling list.  I don't
-- recomend doing this unless you know what you are doing.
fixReplyTo = whenMatch (to_ (Addr "") % "osu-free@lists") filter
   where filter = do m <- match
                     return (Filter ("formail -I\"Reply-To: "++m++"\""))

-- This example shows that conditions can be inverted.
notTest = when ((Not ((said "caring")  .||. 
                      (subject "Hi"))) .&&. 
                (from (Addr "steve")))
          (placeIn (mailbox "notCaring"))

-- Sometimes we want just one condition, but we have multiple actions.
-- In this case, use the also syntax.  It allows mulitple action for 
-- one rule.
alsoTest = when (from (Addr "steve"))
           ((placeIn (mailbox "steve")) `also` 
            (forwardTo [Addr "steve's boss", Addr "steve's friend"]) `also`
            (placeIn (mailbox "backup")))

--End of Rules


-- Example Classifiers, not all of the rules have been captured as
-- classifiers

--Friends/Family
sarah' = simpleClassifyByFrom "sparish"
nwn'   = classifyBySubject "nwn" (mailbox "nwn")
mom'   = classifyByFrom (Addr "Griffinmndm") (mailbox "mom")
dad'   = classifyByFrom (Addr "naturesgifts") (mailbox "dad")
rogan' = classifyByFrom (Addr "creswick") (mailbox "rogan")

--Mailling lists
lkm' = classifyByTo_ (Addr "linux-kernel@vger.kernel.org") 
      (mailbox "linux-kernel")

cvsupdates' = classifyBy ("cvsupdates", 
                         (subject "[forms3-tech]" .&&. subject "[cvs]"))
             (placeIn (mailbox "cvsupdates"))

bugs' = classifyBy ("bugs", (subject "[forms3-tech]" .&&. subject "[jira]"))
       (placeIn (mailbox "bugs"))

forms3Tech' = simpleClassifyByTo_ "forms3-tech"

forms3' = simpleClassifyByTo_ "forms3"

euses' = classifyBy ("euses", ((subject "[eusesnewsletter]") .||. 
                     (to_ (Addr "eusesosugrads"))            .||. 
                     (to_ (Addr "eusesall"))))
         (placeIn (mailbox "euses"))

darcsUsers' = simpleClassifyByTo_ "darcs-users"

darcsDevel' = simpleClassifyByTo_ "darcs-devel"

sbclDevel'  = simpleClassifyByTo_ "sbcl-devel"

ogi' = classifyBy ("ogi", (subject "OGI")) (placeIn (mailbox "csmaillist"))

clispDevel' = simpleClassifyByTo_ "clisp-devel"

csGradTalk' = simpleClassifyByTo_ "cs-grad-talk"

debian' = classifyBy ("debian", 
                     (CheckHeader "^X-Loop: debian.*@lists.debian.org") )
         (placeIn (mailbox "debian"))

csmaillist' = classifyBy ("csmaillist", (subject "[cs-grads]"   .||. 
                         subject "[eecs-grads]"                 .||.
                         to_ (Addr "eecs-grads")))
               (placeIn (mailbox "csmaillist"))

momentum' = classifyBySubject "momentum!" (mailbox "caughtspam")

--rules = do sarah'; nwn'; mom'; dad'; rogan'; lkm'; cvsupdates'; 
--           bugs'; forms3Tech'; forms3'; euses'; darcsUsers';
--           darcsDevel'; sbclDevel'; ogi'; clispDevel'; csGradTalk';
--           debian'; csmaillist'; momentum'
--  --End of Rules

orTest = when (subject "1" .||. 
               subject "2" .&&. 
               subject "3" .&&. 
               subject "4" .&&. 
               subject "5" .&&. 
               subject "6" .&&. 
               subject "7" .&&. 
               subject "8" .&&. 
               subject "9" .&&. 
               subject "10" .&&. 
               subject "11" .&&. 
               subject "12" .&&. 
               subject "13" .&&. 
               subject "14" .&&.
               subject "15")
         (placeIn (mailbox "orTest"))
   
notOrTest = when (Not (subject "1" .||. 
               subject "2" .&&. 
               subject "3" .&&. 
               subject "4" .&&. 
               subject "5" .&&. 
               subject "6" .&&. 
               subject "7" .&&. 
               subject "8" .&&. 
               subject "9" .&&. 
               subject "10" .&&. 
               subject "11" .&&. 
               subject "12" .&&. 
               subject "13" .&&. 
               subject "14" .&&.
               subject "15"))
         (placeIn (mailbox "notOrTest"))
