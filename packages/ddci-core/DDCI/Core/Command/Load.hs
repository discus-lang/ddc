
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.State
import DDC.Core.Module
import DDC.Core.Eval.Env
import DDC.Core.Eval.Name
import DDC.Core.Transform.SpreadX
import DDC.Core.Pretty
import DDC.Core.Parser
import DDC.Core.Parser.Tokens
import qualified DDC.Core.Check         as C
import qualified DDC.Base.Parser        as BP


-- | Parse, check, and single step evaluate an expression.
cmdLoad :: State -> Int -> String -> IO ()
cmdLoad _state lineStart str
 = case loadModule lineStart str of
        Left err  
         -> putStrLn $ renderIndent $ ppr err

        Right _mm 
         -> do  putStrLn "ok"
                return ()


-- LoadModule -----------------------------------------------------------------
data Error
        = ErrorParser BP.ParseError
        | ErrorCheck  (C.Error () Name)
        deriving Show

instance Pretty Error where
 ppr err
  = case err of
        ErrorParser err'  -> ppr err'
        ErrorCheck  err'  -> ppr err'


-- | Parse and type check a core module.
loadModule :: Int -> String -> Either Error (Module () Name)
loadModule lineStart str
 = goParse (lexString lineStart str)
 where
        -- Lex and parse the string.
        goParse toks                
         = case BP.runTokenParser describeTok "<interactive>" pModule toks of
                Left err  -> Left (ErrorParser err)
                Right mm  -> goCheckType (spreadX primKindEnv primTypeEnv mm)

        -- Check the type of the expression.
        goCheckType mm
         = case C.checkModule primDataDefs primKindEnv primTypeEnv mm of
                Left err  -> Left (ErrorCheck err)
                Right mm' -> Right mm'

