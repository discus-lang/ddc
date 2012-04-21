{-# LANGUAGE ExistentialQuantification #-}
module DDC.War.Job where
import qualified DDC.War.Job.CompileDCE as CompileDCE
import qualified DDC.War.Job.CompileDS  as CompileDS
import qualified DDC.War.Job.CompileHS  as CompileHS
import qualified DDC.War.Job.RunDCX     as RunDCX
import qualified DDC.War.Job.RunExe     as RunExe
import qualified DDC.War.Job.Diff       as Diff
import qualified DDC.War.Job.Shell      as Shell
import BuildBox.Pretty
import BuildBox

-- | A chain of jobs to run one after another.
--   If one job in the chain fails then skip the rest.
data Chain
        = Chain [Job]


-- | A single job to run.
data Job
        =  forall spec result. (Show spec, Pretty result)
        => Job spec (Build result)


class (Show spec, Pretty result)
        => Spec spec result | spec -> result 
 where
 build :: spec -> Build result

 make  :: spec -> Job
 make s = Job s (build s)


instance Spec CompileDCE.Spec CompileDCE.Result where
 build = CompileDCE.build

instance Spec CompileDS.Spec  CompileDS.Result where
 build = CompileDS.build

instance Spec CompileHS.Spec  CompileHS.Result where
 build = CompileHS.build

instance Spec RunDCX.Spec     RunDCX.Result where
 build = RunDCX.build

instance Spec RunExe.Spec     RunExe.Result where
 build = RunExe.build

instance Spec Diff.Spec       Diff.Result where
 build = Diff.build

instance Spec Shell.Spec      Shell.Result where
 build = Shell.build
