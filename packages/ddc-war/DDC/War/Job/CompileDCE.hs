
module DDC.War.Job.CompileDCE
        (jobCompileDCE)
where
import DDC.War.Job
import DDC.War.Result
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox.IO.Directory
import BuildBox
import System.FilePath
import System.Directory
import Data.ListUtil
import Control.Monad

-- | Compile a Disciple Core Sea source file.
jobCompileDCE :: Job -> Build [Result]
jobCompileDCE job@(JobCompileDCE
                testName _wayName srcDCE
                buildDir mainCompOut mainCompErr
                mMainBin shouldSucceed)

 = do   needs srcDCE
        
        -- The directory holding the Main.dce file.
        let (srcDir, _srcFile)  = splitFileName srcDCE
                
        -- Touch the .dce files to the build directory to ensure they're built.
        sources <- io
                $  liftM (filter (\f -> isSuffixOf ".dce" f))
                $  lsFilesIn srcDir

        qssystem $ "touch " ++ (catInt " " sources)

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
        
        -- Decide if it was supposed to succeed or fail.
        let result
                | shouldSucceed
                , ExitFailure _ <- code 
                = [ResultUnexpectedFailure]

                | not shouldSucceed
                , ExitSuccess   <- code
                = [ResultUnexpectedSuccess]
                
                | otherwise
                = []
                        
        atomicWriteFile mainCompOut strOut
        atomicWriteFile mainCompErr strErr

        return result
--        (ResultAspect (Time TotalWall `secs` (fromRational $ toRational time)) 
--                : result)
        
