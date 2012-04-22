
module DDC.War.Driver.Chain
        ( runChainWithTChan
        , runJobWithTChan
        , runChain
        , runJob)
where
import DDC.War.Driver
import BuildBox
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM


-- Run a chain of jobs, optionally writing the results to this channel
-- after each job finishes.
runChainWithTChan
        :: Maybe (TChan Result)
        -> Int
        -> Chain
        -> Build [Result]

runChainWithTChan mChannel ixChain (Chain jobs)
 = zipWithM (runJobWithTChan mChannel ixChain) [0..] jobs


-- | Run a job, optionally writing the result to this channel.
runJobWithTChan
        :: Maybe (TChan Result)
        -> Int
        -> Int
        -> Job
        -> Build Result

runJobWithTChan mChannel ixChain ixJob job
 = case mChannel of
        Nothing 
         ->     runJob ixChain ixJob job

        Just channel
         -> do  result  <- runJob ixChain ixJob job
                io $ atomically $ writeTChan channel result
                return result


-- Run ------------------------------------------------------------------------
-- | Run a job chain, returning the results.
runChain :: Int                 -- ^ Index of this chain.
         -> Chain               -- ^ Chain of jobs to run
         -> Build [Result]      -- ^ Job results.

runChain ixChain (Chain jobs)
 = zipWithM (runJob ixChain) [0..] jobs


-- | Run a single job, returning its result.
runJob   :: Int                 -- ^ Index of this chain.
         -> Int                 -- ^ Index of this job of the chain.
         -> Job                 -- ^ The job to run.
         -> Build Result

runJob ixChain ixJob job@(Job spec builder)  
 = do   
        -- Run the job.
        result          <- builder

        -- Convert the result into the product the controller wants.
        let product     = productOfResult spec result
        let jobResult   = Result ixChain ixJob job product

        return jobResult

