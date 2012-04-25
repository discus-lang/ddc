
module DDC.War.Driver.Base
        ( Job           (..)
        , JobId         (..)
        , Chain         (..)
        , Product       (..)
        , Result        (..)
        , Spec       (..))
where
import BuildBox.Pretty
import BuildBox


-- | A single job to run.
--   The exact specification and action is defined by the client.
data Job
        =  forall spec result. Spec spec result
        => Job spec (Build result)

data JobId
        = JobId
        { jobIdName             :: String
        , jobIdWay              :: String
        , jobIdTestName         :: String }
        deriving Show

-- | A chain of jobs to run one after another.
--   Jobs later in the list are dependent on earlier ones, so if a job fails
--   then we skip the rest.
data Chain
        = Chain [Job]


-- | The product that we got when running a job.
--   This is the information that the interactive interface needs to decide
--   how to proceed. 
data Product
        = ProductStatus 
        { productJobId          :: JobId
        , productStatus         :: Doc }

        | ProductDiff   
        { productJobId          :: JobId
        , productDiffRef        :: FilePath
        , productDiffOut        :: FilePath
        , productDiffDiff       :: FilePath }

-- | Description of a job and the product we got from running it.
data Result
        = Result 
        { resultChainIx      :: Int
        , resultJobIx        :: Int
        , resultProduct      :: Product }


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

