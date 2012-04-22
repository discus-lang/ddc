
module DDC.War.Create.CreateMainDS
        (create)
where
import DDC.War.Interface.Config
import DDC.War.Job
import System.FilePath
import Data.List
import Data.Set                                 (Set)
import qualified DDC.War.Job.CompileDS          as CompileDS
import qualified DDC.War.Job.RunExe             as RunExe
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | isSuffixOf "Main.ds" filePath
 = let  
        sourceDir        = takeDirectory  filePath
        buildDir         = sourceDir </> "war-" ++ wayName way
        testName         = filePath

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
        compile          = jobOfSpec $ CompileDS.Spec
                                testName (wayName way) filePath
                                (wayOptsComp way) ["-M40M"]
                                buildDir mainCompStdout mainCompStderr
                                (Just mainBin) shouldSucceed

        -- run the binary
        run              = jobOfSpec $ RunExe.Spec
                                testName (wayName way) filePath mainBin
                                mainRunStdout mainRunStderr

        -- diff errors produced by the compilation
        diffError        = jobOfSpec $ Diff.Spec
                                testName (wayName way) mainErrorCheck
                                mainCompStderr mainCompDiff

        -- diff the stdout of the run
        diffStdout       = jobOfSpec $ Diff.Spec
                                testName (wayName way) mainStdoutCheck
                                mainRunStdout mainStdoutDiff

        -- diff the stderr of the run
        diffStderr       = jobOfSpec $ Diff.Spec
                                testName (wayName way) mainStderrCheck
                                mainRunStderr mainStderrDiff

   in   if Set.member mainSH allFiles
         then Nothing
         else Just $ Chain 
                $  [compile]
                ++ (if shouldSucceed    then [run]        else [diffError])
                ++ (if shouldDiffStdout then [diffStdout] else [])
                ++ (if shouldDiffStderr then [diffStderr] else [])

 | otherwise    = Nothing

