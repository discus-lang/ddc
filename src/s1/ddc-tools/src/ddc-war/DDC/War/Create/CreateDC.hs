

module DDC.War.Create.CreateDC
        (create)
where
import DDC.War.Create.Way
import DDC.War.Driver
import System.FilePath
import DDC.War.Job                              ()
import Data.Set                                 (Set)
import qualified DDC.War.Job.CompileDC          as CompileDC
import qualified DDC.War.Job.RunExe             as RunExe
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


-- | Create test jobs for the test file with the given path.
--     We handle files called Main.dcX, where 'X' specifies the language fragment.
create  :: Way           -- Compilation way.
        -> Set FilePath  -- All files in the same directory as the one we're inspecting.
        -> FilePath      -- Name of the file to inspect.
        -> Maybe Chain   -- Maybe a chain of test jobs to do.

create way allFiles filePath

 -- Check if the file looks like a core file that we can compile into
 -- an executable. If so, then determine the language fragment based
 -- on the file name extension.
 | Just fragment
    <- case takeFileName filePath of
        "Main.dcs"      -> Just $ CompileDC.FragmentSalt
        "Main.dct"      -> Just $ CompileDC.FragmentTetra
        "Main.ds"       -> Just $ CompileDC.FragmentTetra
        _               -> Nothing
  = let
        -- Use the whole filepath as the name of the test.
        testName         = filePath

        -- Directory holding the source file.
        sourceDir        = takeDirectory  filePath

        -- Directory where build products should go.
        buildDir         = sourceDir </> "war-" ++ wayName way

        -- Determine names of the possible control files associated
        -- with this test.
        mainSH            = sourceDir </> "Main.sh"
        mainBin           = buildDir  </> "Main.bin"
        mainCompStdout    = buildDir  </> "Main.compile.stdout"
        mainCompStderr    = buildDir  </> "Main.compile.stderr"
        mainCompDiff      = buildDir  </> "Main.compile.stderr.diff"
        mainRunStdout     = buildDir  </> "Main.run.stdout"
        mainRunStderr     = buildDir  </> "Main.run.stderr"
        mainErrorCheck    = sourceDir </> "Main.error.check"
        mainRunErrorCheck = sourceDir </> "Main.runerror.check"
        mainStdoutCheck   = sourceDir </> "Main.stdout.check"
        mainStdoutDiff    = buildDir  </> "Main.run.stdout.diff"
        mainStderrCheck   = sourceDir </> "Main.stderr.check"
        mainStderrDiff    = buildDir  </> "Main.run.stderr.diff"


        -- Decide what we should do based on the available control files.
        -- If there is no file to check for compilation error, then expect success.
        compShouldSucceed = not $ Set.member mainErrorCheck allFiles

        -- If there is no file to check for run error, then expect success.
        runShouldSucceed  = not $ Set.member mainRunErrorCheck allFiles

        -- If we have an expected stdout file then diff the run stdout output.
        shouldDiffStdout  = Set.member mainStdoutCheck allFiles

        -- If we have an expected stderr file then diff the run stderr output.
        shouldDiffStderr  = Set.member mainStderrCheck allFiles

        -- Define the jobs to run.
        -- compile the .ds into a .bin
        compile         = jobOfSpec (JobId testName (wayName way))
                        $ CompileDC.Spec
                                filePath (wayOptsComp way) fragment
                                buildDir mainCompStdout mainCompStderr
                                (Just mainBin) compShouldSucceed

        -- run the binary.
        run             = jobOfSpec (JobId testName (wayName way))
                        $ RunExe.Spec
                                filePath
                                mainBin []
                                mainRunStdout mainRunStderr
                                runShouldSucceed

        -- diff errors produced by the compilation
        diffError       = jobOfSpec (JobId testName (wayName way))
                        $ Diff.Spec
                                mainErrorCheck
                                mainCompStderr mainCompDiff

        -- diff the stdout of the run
        diffStdout      = jobOfSpec (JobId testName (wayName way))
                        $ Diff.Spec
                                mainStdoutCheck
                                mainRunStdout mainStdoutDiff

        -- diff the stderr of the run
        diffStderr      = jobOfSpec (JobId testName (wayName way))
                        $ Diff.Spec
                                mainStderrCheck
                                mainRunStderr mainStderrDiff

        -- If there is a .sh file then run that instead of using
        -- DDC to compile the executable directly.
   in   if Set.member mainSH allFiles
         then Nothing
         else Just $ Chain
                $  [compile]
                ++ (if compShouldSucceed  then [run]        else [diffError])
                ++ (if shouldDiffStdout   then [diffStdout] else [])
                ++ (if shouldDiffStderr   then [diffStderr] else [])

 -- We don't recognize the file as something that we can compile
 -- into an executable, so just ignore it.
 | otherwise
 = Nothing

