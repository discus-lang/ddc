{-# OPTIONS -fno-warn-unused-matches #-}
-- | Run the nightly DDC build.
module DDC.War.Task.Nightly
        ( Spec   (..)
        , Result (..)
        , build)
where
import BuildBox.Command.System
import BuildBox.Command.File
import BuildBox.Pretty
import BuildBox
--import System.FilePath

data Spec
        = Spec
        { specRemoteSnapshotURL :: String
        , specRemoteRepoURL     :: String 
        , specLocalBuildDir     :: FilePath 
        , specRelPackageDir     :: String
        , specBuildThreads      :: Int }
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
build (Spec urlSnapshot urlRepo dirBuild dirPackage buildThreads)
 = do   
        ensureDir dirBuild
        inDir     dirBuild
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
                inDir dirPackage
                 $ do
{-}                        outLn "* Updating shapshot"
                        (darcsOut, darcsErr) <- ssystem $ "darcs pull -av " ++ urlRepo
                        io $ writeFile "../log/02-darcs.stdout" darcsOut
                        io $ writeFile "../log/02-darcs.stderr" darcsErr

                        outLn "* Writing build config"
                        needs "make"
                        io $ writeFile "make/config-override.mk" 
                           $ unlines ["THREADS = " ++ show buildThreads]
-}
                        outLn "* Building project"
                        needs "Makefile"
                        (makeOut, makeErr) <- ssystem $ "make nightly"
                        io $ writeFile "../log/03-make.stdout" makeOut
                        io $ writeFile "../log/03-make.stderr" makeErr

{-
                        outLn "* Building project"
                        (makeOut, makeErr) <- ssystem "make totallogwar"
                        io $ writeFile "../log/03-make.stdout" makeOut
                        io $ writeFile "../log/03-make.stderr" makeErr

                        -- TODO: make test mode drop summary of test results
                        -- TODO: copy results into log dir

                        -- TODO: scp results to log server
                        -- TODO: mail results
-}
                        return ()



        return ResultSuccess

