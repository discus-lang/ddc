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
--   If one job in the chain fails then we skip the rest.
data Chain
        = Chain [Job]


-- | A single job to run.
data Job
        =  forall spec result. Spec spec result
        => Job spec (Build result)


-- | The product that we got when running a job.
--   This is all the information that the interactive interface needs 
--   to worry about.
data Product
        = ProductStatus Doc

        | ProductDiff   
        { productDiffRef        :: FilePath
        , productDiffOut        :: FilePath
        , productDiffDiff       :: FilePath }


-- Spec -----------------------------------------------------------------------
-- | Job specifications.
class (Show spec, Pretty result)
        => Spec spec result | spec -> result where

 -- | Wrap a specification into a job.
 make    :: spec -> Job
 make s    = Job s (build s)

 -- | Create a builder for this job specification.
 build   :: spec -> Build result

 -- | Make the job product from its result.
 --   This cuts away information that the controller doesn't care about.
 produce :: spec -> result -> Product
 produce _ r = ProductStatus (ppr r)


-- Instances ------------------------------------------------------------------
instance Spec CompileDCE.Spec CompileDCE.Result where
 build          = CompileDCE.build

instance Spec CompileDS.Spec  CompileDS.Result where
 build          = CompileDS.build

instance Spec CompileHS.Spec  CompileHS.Result where
 build          = CompileHS.build

instance Spec RunDCX.Spec     RunDCX.Result where
 build          = RunDCX.build

instance Spec RunExe.Spec     RunExe.Result where
 build          = RunExe.build

instance Spec Shell.Spec      Shell.Result where
 build          = Shell.build

instance Spec Diff.Spec       Diff.Result where
 build          = Diff.build
 produce _ r
  = case r of
        Diff.ResultSame                 -> ProductStatus (ppr r)
        Diff.ResultDiff ref out diff    -> ProductDiff ref out diff

