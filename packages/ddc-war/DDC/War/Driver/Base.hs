
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


-- | A printable job identifier.
data JobId
        = JobId
        { jobIdName             :: String 
        , jobIdWay              :: String }
        deriving Show



-- | A single job to run.
--   The exact specification is defined by the client.
data Job
        =  forall spec result. Spec spec result
        => Job JobId String spec (Build result)

instance Pretty Job where
 ppr (Job jobId actionName spec _)
  = text "Job" <+> text (show jobId) <+> text actionName <+> text (show spec)


-- | A chain of jobs to run one after another.
--   Jobs later in the list are dependent on earlier ones, so if a job fails
--   then we skip the rest.
data Chain
        = Chain [Job]

instance Pretty Chain where
 ppr (Chain jobs)
  =   text "Chain"
  <+> ppr jobs


-- | The product that we got when running a job.
--   This is the information that the interactive interface needs to decide
--   how to proceed. 
data Product
        = ProductStatus 
        { productStatus         :: Doc }

        | ProductDiff   
        { productDiffRef        :: FilePath
        , productDiffOut        :: FilePath
        , productDiffDiff       :: FilePath }

-- | Description of a job and the product we got from running it.
data Result
        = Result 
        { resultChainIx         :: Int
        , resultJobIx           :: Int
        , resultJobId           :: JobId
        , resultJobActionName   :: String
        , resultProduct         :: Product }


-- Spec -----------------------------------------------------------------------
-- | Class of Job specifications.
class (Show spec, Pretty result)
        => Spec spec result | spec -> result where

 jobActionName :: spec -> String

 -- | Wrap a specification into a job.
 jobOfSpec         :: JobId -> spec -> Job
 jobOfSpec jobId s = Job jobId (jobActionName s) s (buildFromSpec s)

 -- | Create a builder for this job specification.
 buildFromSpec    :: spec -> Build result

 -- | Make the job product from its result.
 --   This cuts away information that the controller doesn't care about.
 productOfResult  :: spec -> result -> Product

