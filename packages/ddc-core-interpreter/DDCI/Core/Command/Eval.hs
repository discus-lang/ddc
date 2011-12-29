
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
import qualified DDC.Type.Env                   as Env
import qualified DDCI.Core.Eval.Store           as Store
import qualified Data.Set                       as Set


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
         = case step store x of
             Nothing         -> putStrLn $ show $ text "STUCK!"
             Just (store', x')  
              -> do     putStrLn $ pretty (ppr x')
                        putStrLn $ pretty (ppr store')


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
           in   goStep x store

        goStep x store
         | Just (store', x')    <- step store x 
         = case checkExp Env.empty x' of
            Left err
             -> do    putStrLn $ pretty (ppr x)
                      putStrLn $ pretty (ppr x')
                      putStrLn $ pretty (ppr store)
                      putStrLn "OFF THE RAILS!"
                      putStrLn $ show $ ppr err
                      
            Right (_t, _eff, _clo)
             -> do    putStrLn $ pretty (ppr x)
--                    putStrLn $ pretty (ppr t)
                      goStep x' store'
                      
         | otherwise
         = do   putStrLn $ pretty (ppr x)
                putStrLn $ pretty (ppr store)


