

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


-- | Compile and run .dce files.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 |   takeFileName filePath == "Main.dce"
  || takeFileName filePath == "Main.dcl"
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

        fragment
         | takeExtension filePath == "dce"  = CompileDC.FragmentSalt
         | takeExtension filePath == "dcl"  = CompileDC.FragmentLite
         | otherwise            = error "CreateDC.create: bad fragment"

        -- compile the .ds into a .bin
        compile          = jobOfSpec (JobId testName (wayName way))
                         $ CompileDC.Spec
                                filePath (wayOptsComp way) fragment 
                                buildDir mainCompStdout mainCompStderr
                                (Just mainBin) shouldSucceed

        -- run the binary
        run              = jobOfSpec (JobId testName (wayName way))
                         $ RunExe.Spec
                                filePath 
                                mainBin []
                                mainRunStdout mainRunStderr
                                True

        -- diff errors produced by the compilation
        diffError        = jobOfSpec (JobId testName (wayName way))
                         $ Diff.Spec
                                mainErrorCheck
                                mainCompStderr mainCompDiff

        -- diff the stdout of the run
        diffStdout       = jobOfSpec (JobId testName (wayName way))
                         $ Diff.Spec
                                mainStdoutCheck
                                mainRunStdout mainStdoutDiff

        -- diff the stderr of the run
        diffStderr       = jobOfSpec (JobId testName (wayName way))
                         $ Diff.Spec
                                mainStderrCheck
                                mainRunStderr mainStderrDiff

   in   if Set.member mainSH allFiles
         then Nothing
         else Just $ Chain 
                $  [compile]
                ++ (if shouldSucceed    then [run]        else [diffError])
                ++ (if shouldDiffStdout then [diffStdout] else [])
                ++ (if shouldDiffStderr then [diffStderr] else [])

 | otherwise    = Nothing

