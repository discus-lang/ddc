
module DDCI.Core.Command.Kind
        (cmdShowKind)
where
import DDC.Type.Pretty
import DDC.Type.Check.Exp
import DDC.Type.Parser.Lexer
import DDC.Type.Parser
import DDC.Type.Exp
import Control.Monad

cmdShowKind :: String -> IO ()
cmdShowKind ss
 = case sequence (lexType ss) of
        Nothing         -> putStrLn "lexical error"
        Just toks       
         -> do  putStrLn $ "tokens = " ++ show toks
                showKind_toks toks
        

showKind_toks :: [String] -> IO ()
showKind_toks toks
 = case runParserOfStrings pType toks of
        Left err        -> putStrLn $ "parse error " ++ show err
        Right t 
         -> do  putStrLn $ "type = " ++ (show t)
                showKind_type t

        
showKind_type :: Kind String -> IO ()
showKind_type t
 = case kindOfType t of
        Left err        -> putStrLn $ show $ ppr err
        Right k         -> putStrLn $ show $ ppr k
 