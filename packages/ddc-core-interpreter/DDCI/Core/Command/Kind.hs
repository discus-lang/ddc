{-# OPTIONS -fno-warn-missing-signatures #-}
module DDCI.Core.Command.Kind
        (cmdShowKind)
where
import DDCI.Core.Prim.Name
import DDCI.Core.Prim.Env
import DDC.Type.Pretty
import DDC.Type.Check
import DDC.Type.Parser.Lexer
import DDC.Type.Parser.Tokens
import DDC.Type.Parser
import DDC.Type.Exp
import DDC.Base.Lexer
import qualified DDC.Type.Transform     as T
import qualified DDC.Base.Parser        as BP


cmdShowKind :: String -> IO ()
cmdShowKind ss
        = goParse (lexType Name ss)

goParse toks                
 = case parseType toks of 
        Left err        -> putStrLn $ "parse error " ++ show err
        Right t         -> goCheck t

goCheck t
 = case checkType primEnv (T.spread primEnv t) of
        Left err        -> putStrLn $ show $ ppr err
        Right k         -> putStrLn $ show $ (ppr t <> text " :: " <> ppr k)
 

parseType :: [Token (Tok Name)] -> Either BP.ParseError (Type Name)
parseType toks
        = BP.runTokenParser show "<interactive>" pType toks
