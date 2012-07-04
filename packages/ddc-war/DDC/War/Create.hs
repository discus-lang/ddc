
-- | Creation of test jobs.
--   We decide what to do based on what files are in a directory.
--
--   The following have specicial meaning. 
--   They are in priority order, so if there is both a Main.sh and Main.ds file
--   we run the Main.sh but don't then compile the Main.ds as well.
--
--      Main.sh                 Run this shell script.
--
--      Main.ds                 Compile with DDC and run the executable.
--       Main.error.check        And expect compile failure, diff stdout with this file.
--       Main.runerror.check     And expect run failure.
--       Main.stdout.check       Diff run stdout with this file.
--       Main.stderr.check       Diff run stderr with this file.
--
--      Main.dce                Compile with ddci-core and run the executable.
--       Main.error.check        And expect compile failure, diff stdout with this file.
--       Main.stdout.check       Diff run stdout with this file.
--       Main.stderr.check       Diff run stderr with this file.
--
--      *.ds                    Compile with DDC, but there is no executable produced.
--       *.error.check           And expect compile failure, diff stdout with this file.
--
--      Test.dcx                Run with ddci-core.
--       Test.stdout.check       Diff run stdout with this file.
--       
module DDC.War.Create
        ( Way   (..)
        , create)
where
import DDC.War.Create.Way
import DDC.War.Driver
import Data.Maybe
import Data.Set                                 (Set)
import qualified DDC.War.Create.CreateMainSH    as CreateMainSH
import qualified DDC.War.Create.CreateMainHS    as CreateMainHS
import qualified DDC.War.Create.CreateMainDS    as CreateMainDS
import qualified DDC.War.Create.CreateTestDS    as CreateTestDS
import qualified DDC.War.Create.CreateDCX       as CreateDCX
import qualified DDC.War.Create.CreateDC        as CreateDC


-- | Create job chains based on this file.
create  :: Way                  -- ^ Create tests for this way.
        -> Set FilePath         -- ^ All files in the test directory.
        -> FilePath             -- ^ Create test chains based on this file.
        -> [Chain]

create way allFiles filePath
 =      catMaybes
        [ creat way allFiles filePath
        | creat <- 
                [ CreateMainSH.create
                , CreateMainHS.create
                , CreateMainDS.create
                , CreateTestDS.create
                , CreateDCX.create
                , CreateDC.create  ]]

