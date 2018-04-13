
module DDC.War.Driver.Base
        ( module BuildBox
        , module BuildBox.Pretty
        , Job           (..)
        , JobId         (..)
        , Chain         (..)
        , Product       (..)
        , Result        (..)
        , prettyResult
        , Spec          (..))
where
import BuildBox
import BuildBox.Pretty
import Data.Maybe
import Data.List


-------------------------------------------------------------------------------
-- | A single job to run.
--   The exact specification is defined by the client.
data Job
        =  forall spec result. Spec spec result
        => Job JobId String spec (Build result)

-- | A printable job identifier.
data JobId
        = JobId
        { jobIdName             :: String
        , jobIdWay              :: String }
        deriving Show


instance Pretty Job where
 ppr (Job jobId actionName spec _)
         = string "Job"
         % (string $ show jobId)
         % string actionName
         % string (show spec)


-------------------------------------------------------------------------------
-- | A chain of jobs to run one after another.
--   Jobs later in the list are dependent on earlier ones, so if a job fails
--   then we skip the rest.
data Chain
        = Chain [Job]

instance Pretty Chain where
 ppr (Chain jobs)
  =   string "Chain" %% (hsep $ map ppr jobs)


-------------------------------------------------------------------------------
-- | The product that we got when running a job.
--   This is the information that the interactive interface needs to decide
--   how to proceed.
data Product
        = ProductStatus
        { productStatusMsg      :: Text
        , productStatusSuccess  :: Bool }

        | ProductDiff
        { productDiffRef        :: FilePath
        , productDiffOut        :: FilePath
        , productDiffDiff       :: FilePath }


-------------------------------------------------------------------------------
-- | Description of a job and the product we got from running it.
data Result
        = Result
        { resultChainIx         :: Int
        , resultJobIx           :: Int
        , resultJobId           :: JobId
        , resultJobActionName   :: String
        , resultProduct         :: Product }


-- | Pretty print a job result.
prettyResult :: Int -> String -> Int -> Result -> Text
prettyResult chainsTotal prefix padWidth result
 | Result chainIx jobIx jobId actionName product' <- result
 , JobId  testName wayName                       <- jobId
 = let
        status
         = case product' of
                 ProductStatus s _ -> s
                 ProductDiff{}     -> "diff"

        testName'
         = fromMaybe testName (stripPrefix prefix testName)

   in  hsep
        [  parens
            $ padR (length $ show chainsTotal) (string $ show chainIx)
                % string "."
                % string (show jobIx)
        , padL padWidth (string testName')
        , padL 5        (string wayName)
        , padL 8        (string actionName)
        , status ]


-- Spec -----------------------------------------------------------------------
-- | Class of Job specifications.
class (Show spec, Pretty result)
        => Spec spec result | spec -> result where

 -- | Get a short name to describe the job that this spec describes,
 --   eg "compile" or "run"
 specActionName    :: spec -> String

 -- | Wrap a specification into a job.
 jobOfSpec         :: JobId -> spec -> Job
 jobOfSpec jobId s = Job jobId (specActionName s) s (buildFromSpec s)

 -- | Create a builder for this job specification.
 buildFromSpec    :: spec -> Build result

 -- | Make the job product from its result.
 --   This cuts away information that the controller doesn't care about.
 productOfResult  :: spec -> result -> Product


