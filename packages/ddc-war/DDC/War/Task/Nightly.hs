{-# OPTIONS -fno-warn-unused-matches #-}
-- | Run the nightly DDC build.
module DDC.War.Task.Nightly
        ( Spec   (..)
        , Result (..)
        , build)
where
import BuildBox.Command.Mail
import BuildBox.Command.System
import BuildBox.Command.File
import BuildBox.Command.Environment
import BuildBox.Pretty
import BuildBox.Build
import BuildBox.Time
import BuildBox                 as BuildBox
import Control.Monad
import Data.Char

import System.FilePath.Posix    as Remote
import System.FilePath          as Local


-- Spec -----------------------------------------------------------------------
data Spec
        = Spec
        { -- | URL of DDC snapshot.tgz
          specRemoteSnapshotURL :: String

          -- | URL of DDC repository, used to update the snapshot.
        , specRemoteRepoURL     :: String 

          -- | Use this scratch directory to perform the build.
        , specLocalBuildDir     :: FilePath 

          -- | Number of threads to use when building.
        , specBuildThreads      :: Int


          -- | User and host name to copy logs to eg 'overlord@deluge.ouroborus.net'
        , specLogUserHost       :: Maybe String

          -- | Copy logs into this directory on the server
        , specLogRemoteDir      :: Maybe String

          -- | HTTP address of where the above logs appear
        , specLogRemoteURL      :: Maybe String

          -- | Mailer to use to send build results,
          --   or Nothing if to not send mail.
        , specMailer            :: Maybe Mailer

          -- | Send mail from this address.
        , specMailFrom          :: Maybe String 

          -- | Send mail to this address.
        , specMailTo            :: Maybe String }
        deriving Show


-- Result ---------------------------------------------------------------------
data Result
        = ResultSuccess
        | ResultFailure


instance Pretty Result where
 ppr result
  = case result of
        ResultSuccess   -> text "success"
        ResultFailure   -> text "failure"


-- Build ----------------------------------------------------------------------
-- | Run the nightly DDC build.
build :: Spec -> Build Result
build spec
 = BuildBox.catch 
        (buildProject spec) 
        (\err -> do
                postFailure spec err
                return  ResultFailure)

buildProject :: Spec -> Build Result
buildProject spec
 = do   
        strTime         <- io $ getStampyTime
        let buildDir    = specLocalBuildDir spec Local.</> strTime

        let urlSnapshot  = specRemoteSnapshotURL spec
        let urlRepo      = specRemoteRepoURL     spec
        let buildThreads = specBuildThreads      spec

        ensureDir buildDir
        inDir     buildDir
         $ do 
                outLn "* Creating log directory"
                ensureDir "log"
                
                outLn "* Downloading snapshot"
                (getOut, getErr) <- ssystem $ "wget --progress=bar " ++ urlSnapshot
                io $ writeFile "log/01-wget.stdout" getOut
                io $ writeFile "log/01-wget.stderr" getErr

                needs (takeFileName urlSnapshot)
                outLn "* Unpacking snapshot"
                ssystem $ "tar zxf " ++ takeFileName urlSnapshot

                inDir "ddc-head"
                 $ do
                        outLn "* Updating shapshot"
                        (darcsOut, darcsErr) <- ssystem $ "darcs pull -av " ++ urlRepo
                        io $ writeFile "../log/02-darcs.stdout" darcsOut
                        io $ writeFile "../log/02-darcs.stderr" darcsErr

                        outLn "* Writing build config"
                        needs "make"
                        io $ writeFile "make/config-override.mk" 
                           $ unlines ["THREADS = " ++ show buildThreads]

                        outLn "* Building project"
                        needs "Makefile"
                        (makeOut, makeErr) <- ssystem $ "make nightly"
                        io $ writeFile "../log/03-make.stdout" makeOut
                        io $ writeFile "../log/03-make.stderr" makeErr

                        outLn "* Copying results"
                        needs "war.results"
                        needs "war.failed"
                        ssystem "cp war.results ../log"
                        ssystem "cp war.failed  ../log"

                        return ()

                -- Copy logs to remote host.
                copyLogs spec strTime

                -- Send mail reporting build success including failed tests.
                strFailed       <- io $ readFile "log/war.failed"
                postSuccess spec strTime strFailed

        return ResultSuccess


-- | Copy logs to the remote host
copyLogs :: Spec -> String -> Build ()
copyLogs spec strTime
 | Just userHost <- specLogUserHost  spec
 , Just logDir   <- specLogRemoteDir spec
 = do   
        outLn $ "* Copying logs to " ++ userHost

        let dir = logDir Remote.</> strTime

        -- Make the destination directory.
        sesystemq $ "ssh " ++ userHost ++ " mkdir -p " ++ dir

        -- Copy the files.
        sesystemq $ "scp -r log/* " ++ userHost ++ ":" ++ dir

        return ()

 | otherwise    = return ()


-- | Send mail reporting build success.
postSuccess :: Spec -> String -> String -> Build ()
postSuccess spec strTime strFailed
 | Just mailer          <- specMailer   spec
 , Just addrFrom        <- specMailFrom spec
 , Just addrTo          <- specMailTo   spec
 = do   
        outLn "* Reporting build success"

        -- The overall build succeeded, but some tests might have failed.
        let nFailed     =  length $ lines strFailed

        -- Create message subject.
        hostId          <- getHostId
        let subject     
                | nFailed == 0  
                = "DDC Build Success (" ++ hostId ++ ")"

                | otherwise     
                = "DDC Build Success (" ++ hostId ++ ") with " ++ show nFailed ++ " failed tests"

        -- Create message body.
        let strLog      
                = case specLogRemoteURL spec of
                   Nothing  -> ""
                   Just url -> "Logs are at: " ++ url ++ "/" ++ strTime ++ "\n"

        let body        = strLog ++ strFailed

        -- Send the mail
        mail    <- createMailWithCurrentTime addrFrom addrTo subject body

        sendMailWithMailer mail mailer

-- don't send mail.
postSuccess _ _ _       = return ()


-- | Send mail reporting build failure.
postFailure :: Spec -> BuildError -> Build ()
postFailure spec err
 | Just mailer          <- specMailer   spec
 , Just addrFrom        <- specMailFrom spec
 , Just addrTo          <- specMailTo   spec
 = do   
        outLn "* Reporting build failure"

        -- Create message subject.
        hostId          <- getHostId
        let subject     =  "DDC Build FAILURE (" ++ hostId ++ ")"

        -- Create message body.
        let body        = render $ ppr err

        -- Send the mail
        mail    <- createMailWithCurrentTime addrFrom addrTo subject body

        sendMailWithMailer mail mailer

-- don't send mail
postFailure spec strFailed = return ()


-- | Build a string identifying this host
getHostId :: Build String
getHostId
 = do
        hostName        <- liftM (takeWhile (not . (==) '.')) $ getHostName
        machName        <- liftM (takeWhile (not . isSpace))  $ sesystemq "uname -m"
        osName          <- liftM (takeWhile (not . isSpace))  $ sesystemq "uname"
        return $ hostName ++ "." ++ machName ++ "." ++ osName

