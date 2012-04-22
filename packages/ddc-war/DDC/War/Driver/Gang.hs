
module DDC.War.Driver.Gang
        ( forkChainsIO
        , runChainIO)
where
import DDC.War.Driver.Base
import DDC.War.Driver.Chain
import BuildBox.Build.BuildState
import BuildBox.Control.Gang
import BuildBox
import Control.Concurrent.STM.TChan
import System.Random


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
 = do   uid             <- getUniqueId          -- TODO: buildbox should handle this
                                                --       should not need process id.
        let state       = buildStateDefault uid tmpDir

        runBuildWithState state
         $ runChainWithTChan mChanResult ixChain chain

        return ()


-- | Get a unique(ish) id for this process.
--   The random seeds the global generator with the cpu time in psecs,
--   which should be good enough.
getUniqueId :: IO Integer
getUniqueId
        = randomRIO (0, 1000000000)     
