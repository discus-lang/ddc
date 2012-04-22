
module DDC.War.Create.CreateMainHS
        (create)
where
import DDC.War.Interface.Config
import DDC.War.Driver
import System.FilePath
import DDC.War.Job                              ()
import Data.Set                                 (Set)
import qualified DDC.War.Job.CompileHS          as CompileHS
import qualified DDC.War.Job.RunExe             as RunExe


-- | Compile and run Main.hs files.
--   When we run the exectuable, pass it out build dir as the first argument.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | takeFileName filePath == "Main.hs"
 = let  
        sourceDir       = takeDirectory  filePath
        buildDir        = sourceDir </> "war-" ++ wayName way
        testName        = filePath

        mainBin         = buildDir </> "Main.bin"
        mainCompStdout  = buildDir </> "Main.compile.stdout"
        mainCompStderr  = buildDir </> "Main.compile.stderr"
        mainRunStdout   = buildDir </> "Main.run.stdout"
        mainRunStderr   = buildDir </> "Main.run.stderr"

        compile         = jobOfSpec $ CompileHS.Spec
                                testName (wayName way) filePath []
                                buildDir mainCompStdout mainCompStderr
                                mainBin

        run             = jobOfSpec $ RunExe.Spec
                                testName (wayName way) filePath 
                                mainBin [buildDir]
                                mainRunStdout mainRunStderr
                                True

   in   Just $ Chain [compile, run]

 | otherwise    = Nothing
