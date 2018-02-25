
module DDC.War.Create.CreateMainSH
        (create)
where
import DDC.War.Create.Way
import DDC.War.Driver
import System.FilePath
import Data.Set                                 (Set)
import DDC.War.Job                              ()
import qualified DDC.War.Job.Shell              as Shell
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


-- | Run Main.sh files.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | takeFileName filePath == "Main.sh"
 = let
        sourceDir       = takeDirectory  filePath
        buildDir        = sourceDir </> "war-" ++ wayName way
        testName        = filePath


        mainShellStdout         = buildDir  </> "Main.shell.stdout"
        mainShellStderr         = buildDir  </> "Main.shell.stderr"
        mainShellStderrDiff     = buildDir  </> "Main.compile.stderr.diff"
        mainErrorCheck          = sourceDir </> "Main.error.check"
        shouldSucceed           = not $ Set.member mainErrorCheck allFiles

        shell           = jobOfSpec (JobId testName (wayName way))
                        $ Shell.Spec 
                                filePath sourceDir buildDir
                                mainShellStdout mainShellStderr
                                shouldSucceed

        diffError       = jobOfSpec (JobId testName (wayName way))
                        $ Diff.Spec
                                mainErrorCheck
                                mainShellStderr mainShellStderrDiff

   in   Just $ Chain 
         $  [shell] 
         ++ (if shouldSucceed then [] else [diffError])

 | otherwise    = Nothing
