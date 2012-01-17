
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
import DDC.Type.Equiv
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

        goStore (Just (x, tX, effX, cloX))
         = let  rs      = [ r | UPrim (NameRgn r) _ <- Set.toList $ gatherBound x]
                store   = Store.empty { Store.storeRegions = Set.fromList rs }
           in   goStep store x tX effX cloX

        goStep store x tX effX cloX
         = do   _       <- stepPrint state store x tX effX cloX
                return ()


-- | Parse, check, and single step evaluate an expression.
cmdEval :: State -> String -> IO ()
cmdEval state str
 = cmdParseCheckExp str >>= goStore
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = return ()

        goStore (Just (x, tX, effX, cloX))
         = do   let rs      = [ r | UPrim (NameRgn r) _ <- Set.toList $ gatherBound x]
                let store   = Store.empty { Store.storeRegions = Set.fromList rs }          
                      -- TODO: next region to alloc should be higher than all of these.

                -- Print starting expression.
                when (Set.member TraceEval  $ stateModes state)
                 $ putStrLn $ pretty (text "* STEP: " <> ppr x)

                -- Print starting store.
                when (Set.member TraceStore $ stateModes state)
                 $ putStrLn $ pretty (ppr store)

                goStep store x tX effX cloX

        goStep store0 x0 tX effX cloX
         = go store0 x0
         where go store x
                = do mResult <- stepPrint state store x tX effX cloX
                     case mResult of
                      Nothing           -> return ()
                      Just (store', x') -> go store' x'
         

-- | Perform a single step of evaluation and print what happened.
stepPrint 
        :: State
        -> Store 
        -> Exp ()  Name 
        -> Type    Name
        -> Effect  Name
        -> Closure Name
        -> IO (Maybe (Store, Exp () Name))

stepPrint state store x tX _effX _cloX
 = case step store x of
        StepProgress store' x'
         -> case checkExp primDataDefs Env.empty x' of
             Left err
              -> do putStr $ pretty $ vcat
                        [ text "* OFF THE RAILS!"
                        , ppr err
                        , empty]
                    return $ Nothing
                      
             Right (tX', _effX', _cloX')
              -> do 
                    -- Print intermediate expression.
                    when (Set.member TraceEval  $ stateModes state)
                     $ putStrLn $ pretty (text "* STEP: " <> ppr x')

                    -- Print intermediate store
                    when (Set.member TraceStore $ stateModes state)
                     $ putStrLn $ pretty (ppr store')
                
                    -- Check expression has the same type as before.
                    -- TODO: also check the effect and closure is no greater.
                    let deathT = not $ equivT tX tX'
                    let death  = deathT

                    when deathT
                     $ putStr $ pretty $ vcat
                            [ text "* OFF THE RAILS!"
                            , ppr tX
                            , ppr tX' ]
                
                    if death 
                     then return Nothing
                     else return $ Just (store', x')
    
        StepDone
         -> do  -- Load the final expression back from the store to display.
                putStr $ pretty $ ppr $ traceStore store x

                return Nothing
        
        StepStuck
         -> do  putStr $ pretty $ vcat
                 [ text "* STUCK!"
                 , empty]

                return Nothing

        StepStuckLetrecBindsNotLam
         -> do  putStr $ pretty $ vcat
                 [ text "* STUCK!"
                 , empty]
                
                return Nothing

        StepStuckMistyped err
         -> do  putStr $ pretty $ vcat
                 [ ppr "* OFF THE RAILS!"
                 , ppr err
                 , empty]

                return Nothing

