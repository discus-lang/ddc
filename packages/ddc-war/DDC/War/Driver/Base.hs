
module DDC.War.Driver.Base
        ( Job           (..)
        , Chain         (..)
        , Product       (..)
        , Spec          (..))
where
import BuildBox.Pretty
import BuildBox

-- | A single job to run.
data Job
        =  forall spec result. Spec spec result
        => Job spec (Build result)


-- | A chain of jobs to run one after another.
--   If one job in the chain fails then we skip the rest.
data Chain
        = Chain [Job]


-- | The product that we got when running a job.
--   This is all the information that the interactive interface needs 
--   to worry about.
data Product
        = ProductStatus 
        { productJobName        :: String
        , productWayName        :: String
        , productTestName       :: String
        , productStatus         :: Doc }

        | ProductDiff   
        { productJobName        :: String
        , productWayName        :: String
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

