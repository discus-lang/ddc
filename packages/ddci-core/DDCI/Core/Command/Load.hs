
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.Language
import DDCI.Core.State
import DDC.Core.Load
import DDC.Core.Pretty


-- | Parse, check, and single step evaluate an expression.
--   TODO: check capabilities in module.
cmdLoad :: State -> Int -> String -> IO ()
cmdLoad state lineStart str
 | Language profile     <- stateLanguage state
 = let  toks    = fragmentLex lineStart str
        eModule = loadModule profile "<interactive>"  toks

   in case eModule of
        Left err  
         -> putStrLn $ renderIndent $ ppr err

        Right _mm 
         -> do  putStrLn "ok"
                return ()
