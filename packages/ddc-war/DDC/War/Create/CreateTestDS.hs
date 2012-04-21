
module DDC.War.Create.CreateTestDS
        (create)
where
import DDC.War.Interface.Config
import DDC.War.Job
import System.FilePath
import Data.List
import Data.Set                                 (Set)
import qualified DDC.War.Job.CompileDS          as CompileDS
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | isSuffixOf ".ds" filePath
 = let  
        fileName        = takeFileName filePath
        sourceDir       = takeDirectory  filePath
        buildDir        = sourceDir </> "war-" ++ wayName way
        testName        = filePath

        mainDS          = sourceDir </> "Main.ds"
        mainSH          = sourceDir </> "Main.sh"
        testCompStdout  = buildDir  </> replaceExtension fileName ".compile.stdout"
        testCompStderr  = buildDir  </> replaceExtension fileName ".compile.stderr"
        testCompDiff    = buildDir  </> replaceExtension fileName ".compile.stderr.diff"
        testErrorCheck  = sourceDir </> replaceExtension fileName ".error.check"
        shouldSucceed   = not $ Set.member testErrorCheck allFiles

        -- Compile the .ds file
        compile         = jobOfSpec $ CompileDS.Spec
                                testName (wayName way) filePath
                                (wayOptsComp way) ["-M30M"]
                                buildDir testCompStdout testCompStderr
                                Nothing shouldSucceed

        diffError       = jobOfSpec $ Diff.Spec
                                testName (wayName way) testErrorCheck
                                testCompStderr testCompDiff

   in   -- Don't do anything if there is a Main.ds here.
        -- This other .ds file is probably a part of a larger program.
        if   Set.member mainDS allFiles
          || Set.member mainSH allFiles
           then Nothing
           else Just $ Chain
                        $ [compile] 
                        ++ (if shouldSucceed then [] else [diffError])

 | otherwise    = Nothing
