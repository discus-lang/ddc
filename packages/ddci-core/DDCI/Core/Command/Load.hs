
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.State
import DDC.Core.Load
import DDC.Core.Eval.Profile
import DDC.Core.Eval.Name
import DDC.Core.Pretty


-- | Parse, check, and single step evaluate an expression.
--   TODO: check capabilities in module.
cmdLoad :: State -> Int -> String -> IO ()
cmdLoad _state lineStart str
 = let  toks    = lexString lineStart str
        eModule = loadModule evalProfile "<interactive>"  toks

   in case eModule of
        Left err  
         -> putStrLn $ renderIndent $ ppr err

        Right _mm 
         -> do  putStrLn "ok"
                return ()
