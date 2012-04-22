
module DDC.War.Create.CreateMainDS
        (create)
where
import DDC.War.Interface.Config
import DDC.War.Job
import System.FilePath
import Data.Set                                 (Set)
import qualified DDC.War.Job.CompileDS          as CompileDS
import qualified DDC.War.Job.RunExe             as RunExe
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


-- | Compile and run Main.ds files.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | takeFileName filePath == "Main.ds"
 = let  
        sourceDir          = takeDirectory  filePath
        buildDir           = sourceDir </> "war-" ++ wayName way
        testName           = filePath

        mainSH             = sourceDir </> "Main.sh"

        -- Source ---------------------
        -- If this exists then expect compilation to fail.
        mainErrorCheck     = sourceDir </> "Main.error.check"

        -- If this exists then expect the execuable to fail at runtime.
        mainRunErrorCheck  = sourceDir </> "Main.runerror.check"

        -- Expected output of successful executable.
        mainStdoutCheck    = sourceDir </> "Main.stdout.check"
        mainStderrCheck    = sourceDir </> "Main.stderr.check"

        -- Output ---------------------
        -- Where to put compiler output.
        mainCompStdout     = buildDir  </> "Main.compile.stdout"
        mainCompStderr     = buildDir  </> "Main.compile.stderr"
        mainCompStderrDiff = buildDir  </> "Main.compile.stderr.diff"

        -- Where to put exectuable output.
        mainBin            = buildDir  </> "Main.bin"
        mainRunStdout      = buildDir  </> "Main.run.stdout"
        mainRunStdoutDiff  = buildDir  </> "Main.run.stdout.diff"
        mainRunStderr      = buildDir  </> "Main.run.stderr"
        mainRunStderrDiff  = buildDir  </> "Main.run.stderr.diff"

        -- compile the .ds into a .bin
        --   We expect the compile to fail if there is a Main.error.check file.
        compShouldSucceed  = not $ Set.member mainErrorCheck allFiles
        jobCompile         = jobOfSpec $ CompileDS.Spec
                                testName (wayName way) filePath
                                (wayOptsComp way) ["-M40M"]
                                buildDir mainCompStdout mainCompStderr
                                (Just mainBin) compShouldSucceed

        -- If we expect the compile to fail, 
        --   then diff errors produced by the compilation
        shouldDiffCompError = not $ compShouldSucceed
        jobDiffCompError    = jobOfSpec $ Diff.Spec
                                testName (wayName way) mainErrorCheck
                                mainCompStderr mainCompStderrDiff

        -- If we expect the compile to succeed, then run the resulting binary.
        --   We expect the run to fail if there is a Main.runerror.check file.
        shouldRunExe        = compShouldSucceed
        runShouldSucceed    = not $ Set.member mainRunErrorCheck allFiles
        jobRunExe           = jobOfSpec $ RunExe.Spec
                                testName (wayName way) filePath mainBin
                                mainRunStdout mainRunStderr
                                runShouldSucceed

        -- If we expect the run to succeed,
        --   then diff the stdout of the run.
        shouldDiffStdout    = shouldRunExe && Set.member mainStdoutCheck allFiles
        jobDiffStdout       = jobOfSpec $ Diff.Spec
                                testName (wayName way) mainStdoutCheck
                                mainRunStdout mainRunStdoutDiff

        -- If we expect the run to succeed,
        --   then diff the stderr of the run.
        shouldDiffStderr    = shouldRunExe && Set.member mainStderrCheck allFiles
        jobDiffStderr       = jobOfSpec $ Diff.Spec
                                testName (wayName way) mainStderrCheck
                                mainRunStderr mainRunStderrDiff

   in   if Set.member mainSH allFiles
         then Nothing
         else Just $ Chain 
                $  [jobCompile]
                ++ (if shouldDiffCompError then [jobDiffCompError] else [])
                ++ (if shouldRunExe        then [jobRunExe]        else [])
                ++ (if shouldDiffStdout    then [jobDiffStdout]    else [])
                ++ (if shouldDiffStderr    then [jobDiffStderr]    else [])

 | otherwise    = Nothing


