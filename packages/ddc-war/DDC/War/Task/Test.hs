
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


-- | Run regression tests.
data Spec
        = Spec
        { -- | Start looking for tests from these directories.
          specTestDirs          :: [FilePath]

          -- | Ways to run each test.
        , specWays              :: [Way]

          -- | Number of concurrent threads to use.
        , specThreads           :: Int

          -- | Ask user what to do about unexpected test outputs interactively.
        , specInteractive       :: Bool 

          -- | Pad test names out to this column width in log files.
        , specFormatPathWidth   :: Int

          -- | Write all test results to this file.
        , specResultsFileAll    :: Maybe FilePath

          -- | Write only failed test results to this file
        , specResultsFileFailed :: Maybe FilePath }
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
        currentDir      <- io $ getCurrentDirectory

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

        -- Suppress this prefix from the front of test names when we display them.
        let prefix = currentDir ++ "/"

        -- Run all the chains.
        results <- io $ runChainsWithControllerIO prefix spec chains
        
        -- Write all results to file if we were asked for it
        let chainsTotal = length chains
        let pathWidth   = specFormatPathWidth spec
        let pprResult   = render . Driver.prettyResult chainsTotal prefix pathWidth
        (case specResultsFileAll spec of
          Nothing       -> return ()
          Just file     -> io   $ writeFile file 
                                $ unlines 
                                $ map pprResult
                                $ results)

        -- Write failed results to file if we were asked for it.
        let wasSuccess p
                = case p of
                        Driver.ProductStatus _ True     -> True
                        _                               -> False
        (case specResultsFileFailed spec of 
          Nothing       -> return ()
          Just file     -> io   $ writeFile file
                                $ unlines
                                $ map pprResult
                                $ filter (wasSuccess . Driver.resultProduct)
                                $ results)

        return ResultSuccess


-- | Fork threads to run job chains.
--      We display test results interactivly on the console,
--      as well as allowing the user to interrupt by pressing ENTER.
--
--      In batch mode: if we get a ResultDiff saying a test file is different
--      then just treat it as failed.
--
--      In non-batch mode: if we get a ResultDiff then use the controller
--      to ask the user what to do about it interactively.
--
runChainsWithControllerIO
        :: String               -- ^ Suppress this prefix from the front of test names.
        -> Spec                 -- ^ Build configuration.
        -> [Chain]              -- ^ Chains of jobs to run.
        -> IO [Driver.Result]

runChainsWithControllerIO prefix spec chains
 = do   
        -- Count the total number of chains for the status display.
        let chainsTotal = length chains

        -- Create a new channel to communicate between the test driver  and the
        -- controller. As each test finishes, the driver writes the result to the
        -- channel, and the controller reads the results and displays them 
        -- on the console.
        (chanResult :: TChan Driver.Result)
                <- atomically $ newTChan
        
        -- Fork a gang to run all the job chains.
        gang    <- Driver.forkChainsIO 
                        (specThreads spec) "/tmp"
                        (Just chanResult) chains

        -- Fork the controller to display results and manage user input.
        --   When the controller it done it also writes all the results
        --   it received to an MVar to send them back to the main thread.
        let configController
                = Controller.Config
                { Controller.configFormatPathWidth = specFormatPathWidth spec
                , Controller.configInteractive     = specInteractive spec 
                , Controller.configColoredOutput   = specInteractive spec
                , Controller.configSuppressPrefix  = prefix }

        varResults      <- newEmptyMVar
        forkIO 
         $ do   results <- Controller.controller configController gang chainsTotal chanResult
                putMVar varResults results
         `finally` (putMVar varResults [])

        -- Wait for the controller to finish.
        results <- takeMVar varResults

        -- Wait until the gang is finished running chains, 
        -- or has been killed by the controller.
        joinGang gang

        return results
