module Par (runTests, Result) where

import Control.Concurrent
import Control.Monad (ap, when, replicateM)


type Result = Either [(FilePath, String)] [(FilePath, String)]

--
-- Concurrent test runners
--

-- | Runs a list of actions with a pool of n threads.
runTests
    :: [IO Result]
    -> Int     -- ^ Number of threads.
    -> Bool    -- ^ Keep going even if a test fails?
    -> IO ()
runTests tests nThreads' keepGoing = do

    let nThreads = 1 `max` nThreads'

    testsChan   <- newChan
    resultsChan <- newChan
    failedMV    <- newEmptyMVar

    -- Fork off the test runners.  They don't get the raw MVars and
    -- Chans, just IO actions around them.
    writeList2Chan testsChan $ map Just tests
    onDone <- replicateM nThreads $ do
        mv <- newEmptyMVar
        writeChan testsChan Nothing
        forkOS $ testRunner
                     (writeChan resultsChan Nothing >> putMVar mv ())
                     (if keepGoing then return True
                          else isEmptyMVar failedMV)
                     (\x y -> writeChan resultsChan $ Just (x,y))
                   . permitN 0 =<< (getChanContents testsChan)
        return mv

    -- Run the results printer.
    resultsPrinter
        (\r -> tryPutMVar failedMV [r] >>= \x ->
               if x then return ()
                   else modifyMVar_ failedMV (return . (r:)))
      . permitN (nThreads-1) =<< getChanContents resultsChan


    -- Print out the status of threads every now and again.
    forkOS $ let f = do statuses <- mapM isEmptyMVar onDone
                        putStrLn $ show (length $ filter id statuses) ++
                                   " running threads"
                        when (or statuses) (threadDelay 1000 >> f)
             in f

    -- Print out the results of any failed tests again.
    mapM_ takeMVar onDone
    anyFailed <- fmap not $ isEmptyMVar failedMV
    when anyFailed $ do
        failed <- takeMVar failedMV
        putStrLn $ "\n" ++ show (length failed) ++ " test(s) failed:"
        mapM_ putStrLn failed


-- Executes each test from a list and posts the results for each as
-- long as some condition holds.
testRunner :: IO ()                        -- To be called when done.
           -> IO Bool                      -- Check whether to keep going.
           -> (FilePath -> Bool -> IO ())  -- Post the result of a test.
           -> [IO Result] -> IO ()
testRunner finished noFailures postResult tests =
    guardedMapM noFailures runTest tests >> finished
    where
        runTest x = x >>= either (post False) (post True)
        post _ [] = return ()
        post bool ((fn,_):_) = postResult fn bool

-- Serialise the results of the tests run to stdout.
resultsPrinter :: (FilePath -> IO ()) -> [(FilePath, Bool)] -> IO ()
resultsPrinter reportFailure = mapM_ go
    where
        go (x, True)  = putStrLn ("  " ++ x)
        go (x, False) = putStrLn ("F " ++ x) >> reportFailure x


--
-- Utility functions
--

-- Map an action over a list as long as a monadic predicate remains
-- true.
guardedMapM :: Monad m => m Bool -> (a -> m b) -> [a] -> m [b]
guardedMapM _ _ [] = return []
guardedMapM predM f (x:xs) = do
    p <- predM
    if p then return (:) `ap` f x `ap` guardedMapM predM f xs
        else return []

-- Return the values of a Maybe list, permitting n Nothing values to
-- pass by before ending the list.
permitN :: Int -> [Maybe a] -> [a]
permitN _ [] = []
permitN n (Just x:xs) = x : permitN n xs
permitN n (Nothing:xs) | n > 0 = permitN (n-1) xs
                       | otherwise = []
