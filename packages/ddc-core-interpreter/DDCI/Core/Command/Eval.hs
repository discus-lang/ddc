
module DDCI.Core.Command.Eval
        (cmdStep)
where
import DDCI.Core.Prim
import DDCI.Core.Command.Check
import DDC.Core.Pretty
import DDC.Core.Collect.Free                    ()
import qualified DDCI.Core.Prim.Store           as Store
import qualified DDC.Core.Step                  as C


-- | Parse, check, and single step evaluate an expression.
cmdStep :: String -> IO ()
cmdStep str
 = cmdParseCheckExp str >>= goStore 
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = return ()

        goStore (Just (x, _, _, _))
         = goStep x Store.empty

        goStep x _store
         = case C.step (C.PrimStep primStep) primEnv x Store.empty of
                Nothing         -> putStrLn $ show $ text "STUCK!"
                Just (_s', x')  -> putStrLn $ pretty 100 (ppr x')




