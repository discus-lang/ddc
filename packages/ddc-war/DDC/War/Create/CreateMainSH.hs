
module DDC.War.Create.CreateMainSH
        (create)
where
import DDC.War.Interface.Config
import DDC.War.Job
import System.FilePath
import Data.List
import Data.Set                                 (Set)
import qualified DDC.War.Job.Shell              as Shell
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


-- | Run Main.sh files.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | isSuffixOf "Main.sh" filePath
 = let
        sourceDir       = takeDirectory  filePath
        buildDir        = sourceDir </> "war-" ++ wayName way
        testName        = filePath


        mainShellStdout         = buildDir  </> "Main.shell.stdout"
        mainShellStderr         = buildDir  </> "Main.shell.stderr"
        mainShellStderrDiff     = buildDir  </> "Main.compile.stderr.diff"
        mainErrorCheck          = sourceDir </> "Main.error.check"
        shouldSucceed           = not $ Set.member mainErrorCheck allFiles

        shell           = jobOfSpec $ Shell.Spec 
                                testName (wayName way)
                                filePath sourceDir buildDir
                                mainShellStdout mainShellStderr
                                shouldSucceed

        diffError       = jobOfSpec $ Diff.Spec
                                testName (wayName way) mainErrorCheck
                                mainShellStderr mainShellStderrDiff

   in   Just $ Chain 
         $  [shell] 
         ++ (if shouldSucceed then [] else [diffError])

 | otherwise    = Nothing
