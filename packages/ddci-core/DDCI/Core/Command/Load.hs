
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.Mode
import DDCI.Core.Language
import DDCI.Core.State
import DDC.Core.Load
import DDC.Core.Pretty


-- | Parse, check, and single step evaluate an expression.
--   TODO: check capabilities in module.
cmdLoad :: State -> Source -> String -> IO ()
cmdLoad state source str
 | Language profile     <- stateLanguage state
 = let  toks    = fragmentLex source str
        eModule = loadModule profile (nameOfSource source) toks

   in case eModule of
        Left err  
         -> putStrLn $ renderIndent $ ppr err

        Right mm 
         -> putStrLn $ renderIndent $ ppr mm
