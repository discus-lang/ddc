
module DDC.War.Driver.Gang
        ( forkChainsIO
        , runChainIO)
where
import DDC.War.Driver.Base
import DDC.War.Driver.Chain
import BuildBox.Build.BuildState
import BuildBox.Control.Gang
import Control.Concurrent.STM.TChan


-- | Run some job chains.
forkChainsIO
        :: Int                  -- ^ Number of threads to use.
        -> FilePath             -- ^ Scratch directory.
        -> Maybe (TChan Result) -- ^ Channel to write job results into.
        -> [Chain]              -- ^ Chains of jobs to sun.
        -> IO Gang              -- ^ The gang now running the jobs.

forkChainsIO numThreads dirScratch mChanResult chains
 = do
        -- Fork a gang to run all the job chains.
        gang    <- forkGangActions numThreads
                $  zipWith (runChainIO dirScratch mChanResult)
                        [1..]
                        chains

        return gang


-- | Run a chain of jobs in the IO monad,
--   writing job results to the given channel when they finish.
runChainIO
        :: FilePath
        -> Maybe (TChan Result)
        -> Int
        -> Chain
        -> IO ()

runChainIO tmpDir mChanResult ixChain chain
 = do   let state       = buildStateDefault tmpDir

        runBuildWithState state
         $ runChainWithTChan mChanResult ixChain chain

        return ()
