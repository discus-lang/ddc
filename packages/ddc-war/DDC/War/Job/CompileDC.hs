
module DDC.War.Job.CompileDC
        ( Spec     (..)
        , Fragment (..)
        , Result   (..)
        , resultSuccess
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
import Control.Monad
import Data.List


-- | Use ddci-core to compile/make file a DCE file
data Spec
        = Spec
        { -- | Root source file of the program (the 'Main.ds')
          specFile               :: FilePath 
                       
          -- | Extra DDC options for building in this way.
        , specOptionsDDC         :: [String]

          -- | Language fragment.
        , specFragment           :: Fragment

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


-- | Language fragments that we can compile.
data Fragment
        = FragmentSalt          -- File.dce
        | FragmentLite          -- File.dcl
        deriving Show


-- | Result of a compilation test.
data Result
        = ResultSuccess Seconds
        | ResultUnexpectedFailure
        | ResultUnexpectedSuccess
        deriving Show


resultSuccess :: Result -> Bool
resultSuccess result
 = case result of
        ResultSuccess{} -> True
        _               -> False


instance Pretty Result where
 ppr result
  = case result of 
        ResultSuccess seconds   -> text "success" <+> parens (ppr seconds)
        ResultUnexpectedFailure -> text "failed"
        ResultUnexpectedSuccess -> text "unexpected"


-- Build ----------------------------------------------------------------------
-- | Compile a Disciple Core Sea source file.
build :: Spec -> Build Result
build   (Spec   srcDC optionsDDC _fragment 
                buildDir mainCompOut mainCompErr
                mMainBin shouldSucceed)

 = do   needs srcDC
        needs "bin/ddc"

        -- The directory holding the Main.dce file.
        let (srcDir, _srcFile)  = splitFileName srcDC
                
        -- Touch the .dce and .dcl files to the build directory to ensure they're built.
        sources <- io
                $  liftM (filter (\f -> isSuffixOf ".dce" f || isSuffixOf ".dcl" f))
                $  lsFilesIn srcDir

        ssystemq $ "touch " ++ (intercalate " " sources)

        -- ensure the output directory exists
        ensureDir buildDir

        -- Do the compile.
        let compile
                | Just mainBin  <- mMainBin
                = do    -- If there is an existing binary then remove it.
                        ssystemq $ "rm -f " ++ mainBin

                        -- Build the program.
                        timeBuild
                         $ systemTee False 
                                ("bin/ddc"
                                ++ " "               ++ intercalate " " optionsDDC
                                ++ " -output "       ++ mainBin
                                ++ " -output-dir "   ++ buildDir
                                ++ " -make "         ++ srcDC)
                                ""

                -- Compile the program.
                | otherwise
                = do    timeBuild
                         $ systemTee False
                                ("bin/ddc"
                                ++ " "               ++ intercalate " " optionsDDC
                                ++ " -output-dir "   ++ buildDir
                                ++ " -compile "      ++ srcDC)
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

