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
        = ProductStatus 
        { productJobName        :: String
        , productTestName       :: String
        , productStatus         :: Doc }

        | ProductDiff   
        { productJobName        :: String
        , productTestName       :: String
        , productDiffRef        :: FilePath
        , productDiffOut        :: FilePath
        , productDiffDiff       :: FilePath }


-- Spec -----------------------------------------------------------------------

-- | Class of Job specifications.
class (Show spec, Pretty result)
        => Spec spec result | spec -> result where

 -- | Wrap a specification into a job.
 jobOfSpec        :: spec -> Job
 jobOfSpec s    = Job s (buildFromSpec s)

 -- | Create a builder for this job specification.
 buildFromSpec    :: spec -> Build result

 -- | Make the job product from its result.
 --   This cuts away information that the controller doesn't care about.
 productOfResult  :: spec -> result -> Product


-- Instances ------------------------------------------------------------------
instance Spec CompileDCE.Spec CompileDCE.Result where
 buildFromSpec                  
  = CompileDCE.build
 productOfResult spec result    
  = ProductStatus "compile" (CompileDCE.specTestName spec) (ppr result)

instance Spec CompileDS.Spec  CompileDS.Result where
 buildFromSpec  = CompileDS.build
 productOfResult spec result    
  = ProductStatus "compile" (CompileDS.specTestName spec) (ppr result)

instance Spec CompileHS.Spec  CompileHS.Result where
 buildFromSpec  = CompileHS.build
 productOfResult spec result    
  = ProductStatus "compile" (CompileHS.specTestName spec) (ppr result)

instance Spec RunDCX.Spec     RunDCX.Result where
 buildFromSpec  = RunDCX.build
 productOfResult spec result    
  = ProductStatus "run" (RunDCX.specTestName spec) (ppr result)

instance Spec RunExe.Spec     RunExe.Result where
 buildFromSpec  = RunExe.build
 productOfResult spec result    
  = ProductStatus "run" (RunExe.specTestName spec) (ppr result)

instance Spec Shell.Spec      Shell.Result where
 buildFromSpec  = Shell.build
 productOfResult spec result    
  = ProductStatus "shell" (Shell.specTestName spec) (ppr result)

instance Spec Diff.Spec       Diff.Result where
 buildFromSpec  = Diff.build
 productOfResult spec r
  = case r of
        Diff.ResultSame                 
         -> ProductStatus "diff" (Diff.specTestName spec) (ppr r)

        Diff.ResultDiff ref out diff    
         -> ProductDiff "diff"   (Diff.specTestName spec) ref out diff

