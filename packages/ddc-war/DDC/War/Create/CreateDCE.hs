
module DDC.War.Create.CreateDCE
        (create)
where
import DDC.War.Interface.Config
import DDC.War.Job
import System.FilePath
import Data.List
import Data.Set                                 (Set)
import qualified DDC.War.Job.CompileDCE         as CompileDCE
import qualified DDC.War.Job.RunExe             as RunExe
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


-- | Compile and run .dce files.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | isSuffixOf ".dce" filePath
 = let  
        sourceDir        = takeDirectory  filePath
        buildDir         = sourceDir </> "war-" ++ wayName way
        testName         = sourceDir

        mainSH           = sourceDir </> "Main.sh"
        mainBin          = buildDir  </> "Main.bin"
        mainCompStdout   = buildDir  </> "Main.compile.stdout"
        mainCompStderr   = buildDir  </> "Main.compile.stderr"
        mainCompDiff     = buildDir  </> "Main.compile.stderr.diff"
        mainRunStdout    = buildDir  </> "Main.run.stdout"
        mainRunStderr    = buildDir  </> "Main.run.stderr"

        mainErrorCheck   = sourceDir </> "Main.error.check"
        shouldSucceed    = not $ Set.member mainErrorCheck allFiles

        mainStdoutCheck  = sourceDir </> "Main.stdout.check"
        mainStdoutDiff   = buildDir  </> "Main.run.stdout.diff"
        shouldDiffStdout = Set.member mainStdoutCheck allFiles

        mainStderrCheck  = sourceDir </> "Main.stderr.check"
        mainStderrDiff   = buildDir  </> "Main.run.stderr.diff"
        shouldDiffStderr = Set.member mainStderrCheck allFiles

        -- compile the .ds into a .bin
        compile         = make $ CompileDCE.Spec
                                testName (wayName way) filePath
                                buildDir mainCompStdout mainCompStderr
                                (Just mainBin) shouldSucceed

        -- run the binary
        run             = make $ RunExe.Spec
                                testName (wayName way) filePath mainBin
                                mainRunStdout mainRunStderr

        -- diff errors produced by the compilation
        diffError       = make $ Diff.Spec
                                testName (wayName way) mainErrorCheck
                                mainCompStderr mainCompDiff

        -- diff the stdout of the run
        diffStdout      = make $ Diff.Spec
                                testName (wayName way) mainStdoutCheck
                                mainRunStdout mainStdoutDiff

        -- diff the stderr of the run
        diffStderr      = make $ Diff.Spec
                                testName (wayName way) mainStderrCheck
                                mainRunStderr mainStderrDiff

   in   if Set.member mainSH allFiles
         then Nothing
         else Just $ Chain 
                $ [compile]
                ++ (if shouldSucceed    then [run]        else [diffError])
                ++ (if shouldDiffStdout then [diffStdout] else [])
                ++ (if shouldDiffStderr then [diffStderr] else [])

 | otherwise = Nothing

