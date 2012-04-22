
module DDC.War.Job.CompileDCE
        ( Spec  (..)
        , Result(..)
        , build)
where
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox.Data.Physical
import BuildBox.IO.Directory
import BuildBox.Pretty
import BuildBox
import System.FilePath
import System.Directory
import Control.Monad
import Data.List


-- | Use ddci-core to compile/make file a DCE file
data Spec
        = Spec
        { -- | Name of the test this job is a part of.
          specTestName           :: String

        -- | Name of the way we're running this test.
        , specWayName            :: String
                
        -- | Root source file of the program (the 'Main.ds')
        , specFile               :: FilePath 
                                
        -- | Scratch dir to do the build in.
        , specScratchDir         :: String

        -- | Put what DDC says to stdout here.
        , specCompileStdout      :: FilePath
                
        -- | Put what DDC says to stderr here.
        , specCompileStderr      :: FilePath 

        -- | If Just, then we're making an executable, and put the binary here.
        --   Otherwise simply compile it
        , specMaybeMainBin       :: Maybe FilePath 
                
        -- | True if the compile is expected to succeed, else not.
        , specShouldSucceed      :: Bool }
        deriving Show


data Result
        = ResultSuccess Seconds
        | ResultUnexpectedFailure
        | ResultUnexpectedSuccess
        deriving Show


instance Pretty Result where
 ppr result
  = case result of 
        ResultSuccess seconds   -> text "success" <+> parens (ppr seconds)
        ResultUnexpectedFailure -> text "failed"
        ResultUnexpectedSuccess -> text "unexpected"


-- Build ----------------------------------------------------------------------
-- | Compile a Disciple Core Sea source file.
build :: Spec -> Build Result
build   (Spec   testName _wayName srcDCE
                buildDir mainCompOut mainCompErr
                mMainBin shouldSucceed)

 = do   needs srcDCE
 
        -- The directory holding the Main.dce file.
        let (srcDir, _srcFile)  = splitFileName srcDCE
                
        -- Touch the .dce files to the build directory to ensure they're built.
        sources <- io
                $  liftM (filter (\f -> isSuffixOf ".dce" f))
                $  lsFilesIn srcDir

        qssystem $ "touch " ++ (intercalate " " sources)

        -- ensure the output directory exists
        ensureDir buildDir

        -- Do the compile.
        ddciCoreBin'         <- io $ canonicalizePath "bin/ddci-core"
        let compile
                | Just mainBin  <- mMainBin
                = do    -- If there is an existing binary then remove it.
                        qssystem $ "rm -f " ++ mainBin

                        -- Build the program.
                        timeBuild
                         $ systemTee False 
                                (ddciCoreBin'
                                ++ " -set output "      ++ mainBin
                                ++ " -set outputdir "   ++ buildDir
                                ++ " -make "            ++ srcDCE)
                                ""

                -- Compile the program.
                | otherwise
                = do    timeBuild
                         $ systemTee False
                                (ddciCoreBin'
                                ++ " -set outputdir "   ++ buildDir
                                ++ " -compile "         ++ srcDCE)
                                ""
        (time, (code, strOut, strErr))
                <- compile

        atomicWriteFile mainCompOut strOut
        atomicWriteFile mainCompErr strErr

        case code of
         ExitFailure _
          | shouldSucceed       -> return ResultUnexpectedFailure
        
         ExitSuccess
          | not shouldSucceed   -> return ResultUnexpectedSuccess

         _                      -> return $ ResultSuccess time

