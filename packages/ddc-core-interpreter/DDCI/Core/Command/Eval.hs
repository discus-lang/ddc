
module DDCI.Core.Command.Eval
        (cmdStep)
where
import DDCI.Core.Prim
import DDCI.Core.Command.Check
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Collect
import qualified DDCI.Core.Prim.Store           as Store
import qualified DDC.Core.Step                  as C
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
         = case C.step (C.PrimStep primStep) primEnv x store of
             Nothing         -> putStrLn $ show $ text "STUCK!"
             Just (store', x')  
              -> do     putStrLn $ pretty 100 (ppr x')
                        putStrLn $ pretty 100 (ppr store')

