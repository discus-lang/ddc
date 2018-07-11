
module DDC.Driver.Build
        ( State
        , Job (..)
        , buildWithState
        , buildWithStateIO
        , newStateOfStore
        , addJob, addJobs)
where
import DDC.Driver.Build.State
import DDC.Driver.Build.Run
-- import DDC.Driver.Build.Query
-- import Data.IORef


buildWithStateIO :: State -> IO (Either Error ())
buildWithStateIO state
 = runExceptT
 $ buildWithState state

buildWithState :: State -> S ()
buildWithState state
 = goStart
 where
        goStart
         = do   -- liftIO $ putStrLn "* Starting build"
                goNext

        goNext
         = do   -- liftIO  $ putStrLn "* Next"
                -- liftIO  $ do
                --        (_, jobs) <- readIORef (stateJobs state)
                --        putStrLn $ unlines $ map show jobs

                mJob    <- takeJob state
                case mJob of
                 Nothing  -> error "done"
                 Just job -> goRun job

        goRun job
         = do   -- liftIO  $ do
                --         putStrLn "* Run"
                --         putStrLn $ show job

                bCompleted <- runJob state job
                if bCompleted
                 then goNext
                 else do
                        addJob state job
                        goNext

