
module DDCI.Core.Command.Eval
        ( cmdStep
        , cmdEval)
where
import DDCI.Core.Eval.Step
import DDCI.Core.Eval.Name
import DDCI.Core.Command.Check
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Collect
import DDCI.Core.Eval.Store             (Store)
import qualified DDCI.Core.Eval.Store   as Store
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


-- | Parse, check, and single step evaluate an expression.
cmdStep :: String -> IO ()
cmdStep str
 = cmdParseCheckExp str >>= goStore 
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = return ()

        goStore (Just (x, _, _, _))
         = let  rs      = [ r | UPrim (NameRgn r) _ <- Set.toList $ gatherBound x]
                store   = Store.empty { Store.storeRegions = Set.fromList rs }
           in   goStep x store

        goStep x store
         = do   _       <- stepPrint store x
                return ()


-- | Parse, check, and single step evaluate an expression.
cmdEval :: String -> IO ()
cmdEval str
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
         = do   mResult <- stepPrint store x
                case mResult of
                 Nothing           -> return ()
                 Just (store', x') -> goStep store' x'
         

-- | Perform a single step of evaluation and print what happened.
stepPrint 
        :: Store 
        -> Exp () Name 
        -> IO (Maybe (Store, Exp () Name))

stepPrint store x
 = case step store x of
        StepProgress store' x'
         -> case checkExp Env.empty x' of
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
              -> do putStrLn $ pretty (ppr x)
                    return $ Just (store', x')
    
        StepDone
         -> do  putStr $ pretty $ vcat
                 [ ppr x
                 , ppr store
                 , empty]

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

