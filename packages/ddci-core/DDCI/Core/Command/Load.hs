
module DDCI.Core.Command.Load
        (cmdLoad)
where
import DDCI.Core.State
import DDC.Core.Eval.Name
import DDC.Core.Pretty
import DDC.Core.Parser
import DDC.Core.Parser.Tokens
import qualified DDC.Base.Parser        as BP


-- | Parse, check, and single step evaluate an expression.
cmdLoad :: State -> Int -> String -> IO ()
cmdLoad _state lineStart str
 = goParse (lexString lineStart str)
 where
        -- Lex and parse the string.
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" pModule toks of
                Left err 
                 -> do  putStrLn $ renderIndent $ ppr err
                        return ()
                
                Right _x  -> putStrLn "ok"
