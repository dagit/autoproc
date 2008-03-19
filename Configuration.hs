module Configuration where

-- Anything which is specific to your system should be changed here.

boxPrefix :: String
boxPrefix = "INBOX."

lockDefault :: Bool
lockDefault = True

-- These variables are used to generate the start of .procmailrc
variables :: [(String, String)]
variables = [("SHELL", "/bin/sh"),
             ("PATH", "/usr/local/bin:/usr/bin:$PATH"),
             ("DATE", "`date +%m_%d_%Y`"),
             ("MAILDIR", "$HOME"),
             ("DEFAULT", "$HOME"),
             ("PMDIR", "$HOME/.procmail"),
             ("DUMMY", "`test -d $PMDIR || mkdir $PMDIR`"),
             ("LOGFILE", "$PMDIR/$DATE.log"),
             ("LOGABSTRACT", "on"),
             ("#VERBOSE", "on")]
