
module DDC.War.Driver.Base
        ( Job           (..)
        , JobId         (..)
        , Chain         (..)
        , Product       (..)
        , Result        (..)
        , prettyResult
        , Spec          (..)

        , padL, padR, parens)
where
import BuildBox
import Data.Maybe
import Data.List


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

instance Show Job where
 show (Job jobId actionName spec _)
  = "Job" ++ show jobId ++ actionName ++ show spec


-- | A chain of jobs to run one after another.
--   Jobs later in the list are dependent on earlier ones, so if a job fails
--   then we skip the rest.
data Chain
        = Chain [Job]

instance Show Chain where
 show (Chain jobs)
  =   "Chain" ++ " " ++ show jobs


-- | The product that we got when running a job.
--   This is the information that the interactive interface needs to decide
--   how to proceed.
data Product
        = ProductStatus
        { productStatusMsg      :: String
        , productStatusSuccess  :: Bool }

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


prettyResult :: Int -> String -> Int -> Result -> String
prettyResult chainsTotal prefix padWidth result
        | Result chainIx jobIx jobId actionName product' <- result
        , JobId  testName wayName                       <- jobId
        = let   status
                 = case product' of
                        ProductStatus s _ -> s
                        ProductDiff{}     -> "diff"

                testName'
                 = fromMaybe testName (stripPrefix prefix testName)

          in   parens (padR  (length $ show chainsTotal)
                      (show chainIx)
                   ++ "."
                   ++ (show jobIx))
           ++ " " ++ padL padWidth testName'
           ++ " " ++ padL 5        wayName
           ++ " " ++ padL 8        actionName
           ++ " " ++ status


-- Spec -----------------------------------------------------------------------
-- | Class of Job specifications.
class (Show spec, Show result)
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


padR n s = replicate (n - length s) ' ' ++ s
padL n s = s ++ replicate (n - length s) ' '
parens s = "(" ++ s ++ ")"

