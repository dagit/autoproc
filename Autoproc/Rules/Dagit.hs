module Autoproc.Rules.Dagit where

import Autoproc.Classifier

import Control.Monad.Writer hiding (when)

{- | Any rules that you create need to end up in the rules list.  Other
 than that, feel free to define your own rules using these rules an
 examples.

 A rule is something of the form:

> when condition action

Examples of condition might include:

> (from (Addr "foo@bar"))
> (subject "Hi again")

And example actions are things like:

> (insertMbox "steve")@, @(forward [Addr "friend@yahoo.com"])

 I have created some aliases for commonly used constructions
 For example, @simpleSortByFrom "joe"@, is equivalent to:

> when (from (Addr "joe")) (insertMbox "joe")

 For a full list of what is possible, check the "Autoproc.Classifier" module. -}
--Rules start here:
dagitRules :: Writer [CExp] ()
dagitRules = do spamc; spamcheck; sarah; mom; dad; rogan; lkm; cvsupdates;
                     cdspaper; bugs; forms3Tech; forms3; euses; darcsUsers;
                     darcsDevel; sbclDevel; ogi; clispDevel; csGradTalk;
                     classes; nwn; debian; csmaillist; momentum; fixReplyTo; dagitDefaultRule

-- | I use this rule to make sure any mail that is not sorted goes into
-- my mail spool. It uses "Autoproc.Classifier"'s 'defaultRule'
dagitDefaultRule :: Writer [CExp] ()
dagitDefaultRule = defaultRule "/var/mail/dagit"

--Friends/Family
-- | If the email address contains "sparish", then place the email into a folder
-- by the name "sparish".  Actually, the name of the mailbox is created by
-- appending boxPrefix which is defined in the Configuration module.
sarah :: Writer [CExp] ()
sarah = simpleSortByFrom "sparish"

-- | Similar to 'sarah', except we are sorting based on the subject line,
-- and giving the mailbox.  As above, boxPrefix will be added to "nwn".
nwn :: Writer [CExp] ()
nwn   = subjectToMbox "nwn" "nwn"
-- If the email address contains Griffinmndm then the email is from Mom.
mom :: Writer [CExp] ()
mom   = addressToMbox "Griffinmndm" "mom"
dad :: Writer [CExp] ()
dad   = addressToMbox "naturesgifts" "dad"
rogan :: Writer [CExp] ()
rogan = addressToMbox "creswick" "rogan"

--Mailing lists
lkm :: Writer [CExp] ()
lkm = toAddressToMbox "linux-kernel@vger.kernel.org" "linux-kernel"

-- This is an example of the general syntax.  The above examples are converted
-- to an analogous when statement.

cvsupdates :: Writer [CExp] ()
cvsupdates =  subjectsToMbox ["\\[forms3-tech\\]", "\\[cvs\\]"] "cvsupdates"

cdspaper :: Writer [CExp] ()
cdspaper = subjectsToMbox ["\\[CDs Paper Update\\]"] "cdpaper"

bugs :: Writer [CExp] ()
bugs = subjectsToMbox ["\\[forms3-tech\\]", "\\[jira\\]"] "bugs"

forms3Tech :: Writer [CExp] ()
forms3Tech = simpleSortByTo_ "forms3-tech"

forms3 :: Writer [CExp] ()
forms3 = simpleSortByTo_ "forms3"

euses :: Writer [CExp] ()
euses = when ((subject "\\[eusesnewsletter\\]") .||.
              (to_ (Addr "eusesosugrads"))  .||.
              (to_ (Addr "eusesall")))
        (insertMbox "euses")

darcsUsers :: Writer [CExp] ()
darcsUsers = simpleSortByTo_ "darcs-users"

darcsDevel :: Writer [CExp] ()
darcsDevel = simpleSortByTo_ "darcs-devel"

sbclDevel :: Writer [CExp] ()
sbclDevel  = simpleSortByTo_ "sbcl-devel"

ogi :: Writer [CExp] ()
ogi        = subjectToMbox "OGI" "csmaillist"

clispDevel :: Writer [CExp] ()
clispDevel = simpleSortByTo_ "clisp-devel"

csGradTalk :: Writer [CExp] ()
csGradTalk = simpleSortByTo_ "cs-grad-talk"

-- This rule has a custom header check.  It checks the header of the
-- email for a line that begins with "X-Loop: ...".
-- People familiar with regular expressions will recognize the meaning.
-- of ^ and .*
debian :: Writer [CExp] ()
debian     = when (CheckHeader "^X-Loop: debian.*@lists.debian.org")
             (insertMbox "debian")

csmaillist :: Writer [CExp] ()
csmaillist = when (subject "\\[cs-grads\\]"   .||.
                   subject "\\[eecs-grads\\]" .||.
                   to_ (Addr "eecs-grads"))
             (insertMbox "csmaillist")

--Class lists

-- This is a rather sophisticated example demonstrating matching.
-- When the % operator is used, the text that matches the regular
-- expression on the right hand side of the %, is stored in the variable
-- match.  This requires whenMatch instead of when.
classes :: Writer [CExp] ()
classes    = whenMatch (((to_ (Addr "class-")) % ".*@") `refineBy`
                        ((CheckMatch "()") % "[^@]+"))
             (placeInUsingMatch (mailBoxFromMatch match))

-- Example showing that usage of match is checked using the type
-- system.  In this example match would not have a value because the
-- operator % has not been used in the condition.
--test = when (to_ (Addr "test"))
--      (placeInUsingMatch (mailbox match))

--spam rules
-- | A filter is a special action that transforms the email for
-- the benefit of future rules.  This particular rule,
-- hands the email off to spam assassin so that it can be checked for
-- signs of spam.
spamc :: Writer [CExp] ()
spamc     = when Always (Filter "/usr/local/bin/spamc")

-- isSpam and spamLevel are special conditions for use with SpamAssassin.
spamcheck :: Writer [CExp] ()
spamcheck = when (isSpam       .||.
                 (spamLevel 3) .||.
                 (from (Addr "nationalmkt@planters.net")))
            (insertMbox "caughtspam")

momentum :: Writer [CExp] ()
momentum  = subjectToMbox "momentum!" "caughtspam"

--Random Examples
-- | An example that demonstrates forwarding an email.
sharing :: Writer [CExp] ()
sharing   = when (said "caring" .&&. from (Addr "ecards"))
            (forwardTo [(Addr "dagit@codersbase.com"),
                      (Addr "thedagit@hotmail.com")])

-- | This rules "fixes" the reply-to header of a mailing list.  I don't
-- recommend doing this unless you know what you are doing.
fixReplyTo :: Writer [CExp] ()
fixReplyTo = whenMatch (to_ (Addr "") % "osu-free@lists") filter'
   where filter' = do m <- match
                      return $ Filter $ "formail -I\"Reply-To: "++m++"\""

-- This example shows that conditions can be inverted.
notTest :: Writer [CExp] ()
notTest = when ((Not ((said "caring")  .||.
                      (subject "Hi"))) .&&.
                (from (Addr "steve")))
          (insertMbox "notCaring")

-- | Sometimes we want just one condition, but we have multiple actions.
-- In this case, use the also syntax.  It allows multiple action for
-- one rule.
alsoTest :: Writer [CExp] ()
alsoTest = when (from (Addr "steve"))
           ((insertMbox "steve") `also`
            (forwardTo [Addr "steve's boss", Addr "steve's friend"]) `also`
            (insertMbox "backup"))

--End of Rules


-- Example Classifiers, not all of the rules have been captured as
-- classifiers

--Friends/Family
sarah' :: Writer [CExp] ()
sarah' = simpleClassifyByFrom "sparish"
nwn' :: Writer [CExp] ()
nwn'   = simpleClassifyBySubject "nwn"
mom' :: Writer [CExp] ()
mom'   = classifyByFromAddr "Griffinmndm" "mom"
dad' :: Writer [CExp] ()
dad'   = classifyByFromAddr "naturesgifts" "dad"
rogan' :: Writer [CExp] ()
rogan' = classifyByFromAddr "creswick" "rogan"

--Mailing lists
lkm' :: Writer [CExp] ()
lkm' = classifyByTo_ (Addr "linux-kernel@vger.kernel.org")
      (mailbox "linux-kernel")

---------------------------------------------------------

-- TODO: do some refactoring here

ogi' :: Writer [CExp] ()
ogi' = classifyBy ("ogi",
                   (subject "OGI"))
       (insertMbox "csmaillist")

debian' :: Writer [CExp] ()
debian' = classifyBy ("debian",
                     (CheckHeader "^X-Loop: debian.*@lists.debian.org") )
         (insertMbox "debian")

---

cvsupdates' :: Writer [CExp] ()
cvsupdates' = classifyBy ("cvsupdates",
                         (subject "[forms3-tech]" .&&.
                                  subject "[cvs]"))
             (insertMbox "cvsupdates")

bugs' :: Writer [CExp] ()
bugs' = classifyBy ("bugs",
                    (subject "[forms3-tech]" .&&.
                             subject "[jira]"))
       (insertMbox "bugs")

csmaillist' :: Writer [CExp] ()
csmaillist' = classifyBy ("csmaillist", (subject "[cs-grads]" .||.
                         subject "[eecs-grads]"               .||.
                         to_ (Addr "eecs-grads")))
               (insertMbox "csmaillist")

euses' :: Writer [CExp] ()
euses' = classifyBy ("euses", ((subject "[eusesnewsletter]") .||.
                     (to_ (Addr "eusesosugrads"))            .||.
                     (to_ (Addr "eusesall"))))
         (insertMbox "euses")

--------------------------------------------------------------


forms3Tech' :: Writer [CExp] ()
forms3Tech' = simpleClassifyByTo_ "forms3-tech"

forms3' :: Writer [CExp] ()
forms3' = simpleClassifyByTo_ "forms3"

darcsUsers' :: Writer [CExp] ()
darcsUsers' = simpleClassifyByTo_ "darcs-users"

darcsDevel' :: Writer [CExp] ()
darcsDevel' = simpleClassifyByTo_ "darcs-devel"

sbclDevel' :: Writer [CExp] ()
sbclDevel'  = simpleClassifyByTo_ "sbcl-devel"

clispDevel' :: Writer [CExp] ()
clispDevel' = simpleClassifyByTo_ "clisp-devel"

csGradTalk' :: Writer [CExp] ()
csGradTalk' = simpleClassifyByTo_ "cs-grad-talk"

momentum' :: Writer [CExp] ()
momentum' = classifyBySubject "momentum!" (mailbox "caughtspam")

orTest :: Writer [CExp] ()
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
         (insertMbox "orTest")

notOrTest :: Writer [CExp] ()
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
         (insertMbox "notOrTest")
