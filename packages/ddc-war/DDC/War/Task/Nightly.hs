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
import BuildBox
--import System.FilePath
import Control.Monad
import Data.Char

data Spec
        = Spec
        { specRemoteSnapshotURL :: String
        , specRemoteRepoURL     :: String 
        , specLocalBuildDir     :: FilePath 
        , specRelPackageDir     :: String
        , specBuildThreads      :: Int

        , specMailer            :: Maybe Mailer
        , specMailFrom          :: String 
        , specMailTo            :: String }
        deriving Show


-- TODO: add log files to the ResultSuccess constructor
data Result
        = ResultSuccess


instance Pretty Result where
 ppr result
  = case result of
        ResultSuccess   -> text "success"


-- | Run the nightly DDC build.
build :: Spec -> Build Result
build spec
 = do   
        ensureDir (specLocalBuildDir spec)
        inDir     (specLocalBuildDir spec)
         $ do 
{-}                outLn "* Creating log directory"
                ensureDir "log"
                
                outLn "* Downloading snapshot"
                (getOut, getErr) <- ssystem $ "wget --progress=bar " ++ urlSnapshot
                io $ writeFile "log/01-wget.stdout" getOut
                io $ writeFile "log/01-wget.stderr" getErr

                needs (takeFileName urlSnapshot)
                outLn "* Unpacking snapshot"
                ssystem $ "tar zxf " ++ takeFileName urlSnapshot
-}
                inDir (specRelPackageDir spec)
                 $ do
{-}                        outLn "* Updating shapshot"
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
-}
                        outLn "* Copying results"
                        needs "war.results"
                        needs "war.failed"
                        ssystem "cp war.results ../log"
                        ssystem "cp war.failed  ../log"

                        return ()

                outLn "* Sending mail"
                (case specMailer spec of
                  Nothing       -> return ()
                  Just mailer   -> build_mail spec mailer)

        return ResultSuccess

build_mail :: Spec -> Mailer -> Build ()
build_mail spec mailer
 = do   
        -- Create message subject.
        hostName        <- getHostName
        machName        <- liftM (takeWhile (not . isSpace)) $ sesystemq "uname -m"
        osName          <- liftM (takeWhile (not . isSpace)) $ sesystemq "uname"

        let subject     
                =  "DDC Build Success"
                ++ " (" ++ hostName ++ "." ++ machName ++ "." ++ osName ++ ")" 

        -- Create message body.
        resultsFailed   <- io $ readFile "log/war.failed"
        let body        = resultsFailed

        -- Send the mail
        mail    <- createMailWithCurrentTime 
                        (specMailFrom spec) (specMailTo spec) subject body

        let str = render $ renderMail mail
        io $ writeFile "war.mail" str

        sendMailWithMailer mail mailer


