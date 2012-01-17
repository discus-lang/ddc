
module DDCI.Core.Command.Eval
        ( cmdStep
        , cmdEval)
where
import DDCI.Core.Stats.Trace
import DDCI.Core.Eval.Env
import DDCI.Core.Eval.Step
import DDCI.Core.Eval.Name
import DDCI.Core.Command.Check
import DDCI.Core.State
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Collect
import Control.Monad
import DDCI.Core.Eval.Store             (Store)
import qualified DDCI.Core.Eval.Store   as Store
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


-- | Parse, check, and single step evaluate an expression.
cmdStep :: State -> String -> IO ()
cmdStep state str
 = cmdParseCheckExp str >>= goStore 
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = return ()

        goStore (Just (x, _, _, _))
         = let  rs      = [ r | UPrim (NameRgn r) _ <- Set.toList $ gatherBound x]
                store   = Store.empty { Store.storeRegions = Set.fromList rs }
           in   goStep store x

        goStep store x
         = do   _       <- stepPrint state store x
                return ()


-- | Parse, check, and single step evaluate an expression.
cmdEval :: State -> String -> IO ()
cmdEval state str
 = cmdParseCheckExp str >>= goStore
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = return ()

        goStore (Just (x, _, _, _))
         = let  rs      = [ r | UPrim (NameRgn r) _ <- Set.toList $ gatherBound x]
                store   = Store.empty { Store.storeRegions = Set.fromList rs }          
                      -- TODO: next region to alloc should be higher than all of these.
           in   goStep store x

        goStep store x
         = do   mResult <- stepPrint state store x
                case mResult of
                 Nothing           -> return ()
                 Just (store', x') -> goStep store' x'
         

-- | Perform a single step of evaluation and print what happened.
stepPrint 
        :: State
        -> Store 
        -> Exp () Name 
        -> IO (Maybe (Store, Exp () Name))

stepPrint state store x
 = case step store x of
        StepProgress store' x'
         -> case checkExp primDataDefs Env.empty x' of
             Left err
              -> do putStr $ pretty $ vcat
                        [ ppr x
                        , ppr x'
                        , ppr store
                        , text "OFF THE RAILS!"
                        , ppr err
                        , empty]
                    return $ Nothing
                      
             Right (_t, _eff, _clo)
              -> do 
                    -- Print intermediate expression.
                    when (Set.member TraceEval $ stateModes state)
                     $ putStrLn $ pretty (ppr x)

                    -- TODO: check expression has same type as before,
                    --       also check it has a smaller effect and closure.
                
                    return $ Just (store', x')
    
        StepDone
         -> do  -- TODO: optionally print final exp and store.
                -- putStr $ pretty $ vcat
                -- [ ppr x
                -- , ppr store
                -- , empty]

                -- Load the final expression back from the store to display.
                putStr $ pretty $ ppr $ traceStore store x

                return Nothing
        
        StepStuck
         -> do  putStr $ pretty $ vcat
                 [ ppr x
                 , ppr store
                 , text "STUCK!"
                 , empty]

                return Nothing

        StepStuckMistyped err
         -> do  putStr $ pretty $ vcat
                 [ ppr x
                 , ppr store
                 , ppr "OFF THE RAILS!"
                 , ppr err
                 , empty]

                return Nothing

        StepStuckLetrecBindsNotLam
         -> do  putStr $ pretty $ vcat
                 [ ppr x
                 , ppr store
                 , text "CRASH AND BURN!"
                 , empty]
                
                return Nothing

