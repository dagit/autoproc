module Main (main) where

import Autoproc.Run
import Autoproc.Rules.Dagit

import Prelude hiding (catch)
import System.IO (openFile, IOMode(WriteMode), hClose)
import System.Posix.Process (executeFile, forkProcess, createSession, getProcessStatus)
import System.Process (runProcess, waitForProcess)
import System.Directory (getAppUserDataDirectory, getModificationTime)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Exception (bracket, catch)
import Control.Monad (when)
import System.Exit (ExitCode(..), exitWith)
import Control.Applicative ((<$>))

-- | The entry point into autoproc. Attempts to compile @~/.autoproc/autoproc.hs@
-- for autoproc, and if it doesn't find one, just compiles the default.
-- This code and method is totally stolen from XMonad. Thanks guys!
main :: IO ()
main = catch (buildLaunch) (\_ -> autoprocMain $ dagitRules)

-- | Build "~/.autoproc/autoproc.hs" with GHC, then execute it.  If there are no
-- errors, this function does not return.  An exception is raised in any of
-- these cases:
--   * ghc missing
--   * ~/.autoproc/autoproc.hs missing
--   * autoproc.hs fails to compile
--      ** wrong ghc in path (fails to compile)
--      ** type error, syntax error, ..
--   * Missing autoproc/AutoprocContrib modules due to ghc upgrade
--
buildLaunch :: IO ()
buildLaunch = do
    recompile True
    dir  <- getAutoprocDir
    executeFile (dir ++ "/autoproc") False [] Nothing
    return ()

-- | Return the path to @~\/.autoproc@.
getAutoprocDir :: MonadIO m => m String
getAutoprocDir = liftIO $ getAppUserDataDirectory "autoproc"

-- | 'recompile force', recompile @~\/.autoproc\/autoproc.hs@ when any of the
-- following apply:
--      * force is True
--      * the autoproc executable does not exist
--      * the autoproc executable is older than autoproc.hs
--
-- The -i flag is used to restrict recompilation to the autoproc.hs file only.
--
-- Compilation errors (if any) are logged to ~\/.autoproc\/autoproc.errors.  If
-- GHC indicates failure with a non-zero exit code, an xmessage displaying
-- that file is spawned.
--
-- False is returned if there are compilation errors.
recompile :: MonadIO m => Bool -> m Bool
recompile force = liftIO $ do
    dir <- getAutoprocDir
    let binn = "autoproc"
        bin  = dir ++ "/" ++ binn
        base = dir ++ "/" ++ "autoproc"
        err  = base ++ ".errors"
        src  = base ++ ".hs"
    srcT <- getModTime src
    binT <- getModTime bin
    if (force || srcT > binT)
      then do
        status <- bracket (openFile err WriteMode) hClose $ \h -> do
            waitForProcess =<< runProcess "ghc" ["--make", "autoproc.hs", "-i", "-no-recomp", "-v0", "-o",binn] (Just dir)
                                    Nothing Nothing Nothing (Just h)

        -- now, if it fails, run xmessage to let the user know:
        when (status /= ExitSuccess) $ do
            ghcErr <- readFile err
            let msg = unlines $
                    ["Error detected while loading autoproc configuration file: " ++ src]
                    ++ lines ghcErr ++ ["","Please check the file for errors."]
            doubleFork $ executeFile "xmessage" True ["-default", "okay", msg] Nothing
        return (status == ExitSuccess)
      else return True
 where getModTime f = catch (Just <$> getModificationTime f) (const $ return Nothing)

-- | Double fork and execute an IO action (usually one of the exec family of
-- functions)
doubleFork :: MonadIO m => IO () -> m ()
doubleFork m = liftIO $ do
    pid <- forkProcess $ do
        forkProcess (createSession >> m)
        exitWith ExitSuccess
    getProcessStatus True False pid
    return ()