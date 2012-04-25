
module DDC.War.Task.Test
        ( Spec          (..)
        , Result        (..)
        , build)
where
import DDC.War.Create

import qualified DDC.War.Interface.Controller   as Controller

import DDC.War.Driver                           (Chain(..))
import qualified DDC.War.Driver                 as Driver
import qualified DDC.War.Driver.Gang            as Driver

import BuildBox.Control.Gang
import BuildBox.IO.Directory
import BuildBox.Pretty
import BuildBox

import System.Directory
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Control.Exception
import Data.List

import qualified Data.Sequence          as Seq
import qualified Data.Foldable          as Seq
import qualified Data.Set               as Set
import qualified Data.Traversable       as Seq


data Spec
        = Spec
        { specTestDirs          :: [FilePath]
        , specWays              :: [Way]
        , specThreads           :: Int
        , specFormatPathWidth   :: Int
        , specBatch             :: Bool }
        deriving Show


data Result
        = ResultSuccess
        deriving Show


instance Pretty Result where
 ppr result
  = case result of
        ResultSuccess   -> text "success"


build :: Spec -> Build Result
build spec
 = do
        -- All the starting test directories from the command line.
        testDirs       <- io $ mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
                        $ specTestDirs spec

        -- Trace all the files reachable from these directories.
        testFilesRaw   <- io $ liftM (join . Seq.fromList)
                        $  mapM traceFilesFrom testDirs
                
        -- Canonicalize all the paths and put them in a set (which sorts them)
        testFilesSet   <- io $ liftM (Set.fromList . Seq.toList)
                        $  Seq.mapM canonicalizePath
                        $  testFilesRaw

        -- Skip over files with 'skip' in the path,
        --  and don't decend into our own build dirs.
        let testFilesSorted
                = filter (not . isInfixOf "skip-")
                $ filter (not . isInfixOf "-skip")
                $ filter (not . isInfixOf "war-")
                $ Set.toList testFilesSet

        let testFilesSortedSet
                = Set.fromList testFilesSorted

        -- Create test chains based on the files we have.
        let ways'
                = case specWays spec of
                   []   -> [Way "std" [] []]
                   ways -> ways

        let chains :: [Chain]
            chains = concat 
                [ concat $ map (\way -> create way testFilesSortedSet file) ways'
                | file <- testFilesSorted]

        -- Channel for threads to write their results to.
        (chanResult :: Controller.ChanResult)
                <- io $ atomically $ newTChan

        -- Run all the chains.
        results <- io $ runChains spec chanResult chains
        
        -- TODO: gather up failed tests and write to file.

        return ResultSuccess


-- | Run some job chains.
runChains
        :: Spec                    -- ^ war configuration
        -> Controller.ChanResult   -- ^ channel to write job results to
        -> [Chain]                 -- ^ chains of jobs
        -> IO [Driver.Result]

runChains spec chanResult chains
 = do   
        -- Count the total number of chains for the status display.
        let chainsTotal = length chains
        
        let configController
                = Controller.Config
                { Controller.configFormatPathWidth      = specFormatPathWidth spec
                , Controller.configBatch                = specBatch spec }

        -- Fork a gang to run all the job chains.
        gang    <- Driver.forkChainsIO 
                        (specThreads spec) ("/tmp")
                        (Just chanResult) chains

        -- Fork the gang controller that manages the console and handles
        -- user input.
        varResults      <- newEmptyMVar
        jobResults      
         <- forkIO 
         $ do   results <- Controller.controller configController gang chainsTotal chanResult
                putMVar varResults results
         `finally` (putMVar varResults [])


        -- Wait for the controller to finish.
        results <- takeMVar varResults

        -- Wait until the gang is finished running chains, 
        -- or has been killed by the controller.
        joinGang gang

        return results
        
