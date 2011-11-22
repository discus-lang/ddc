
module DDCI.Core.Command.Type
        (cmdShowType)
where
import DDCI.Core.Token
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import qualified DDC.Core.Parser.Lexer  as XP
import qualified DDC.Core.Parser.Tokens as XP
import qualified DDC.Core.Parser        as XP
import Control.Monad


-- | Show the type of an expression.
cmdShowType :: String -> IO ()
cmdShowType ss
 = case sequence (XP.lexExp ss) of
        Nothing         -> putStrLn "lexical error"
        Just toks       -> showType_toks $ map Token toks


showType_toks :: [Token] -> IO ()
showType_toks toks                
 = case parseExp toks of 
        Left err        -> putStrLn $ "parse error " ++ show err
        Right t         -> showType_exp t

        
showType_exp :: Exp a Token p -> IO ()
showType_exp x
 = case typeOfExp x of
--        Left err        -> putStrLn $ show $ ppr err
        t         -> putStrLn $ show $ (ppr x <> text " :: " <> ppr t)
 

parseExp :: [Token] -> Either XP.ParseError (Exp () Token p)
parseExp toks
 = let  tokenTable      = XP.liftTokens stringOfToken tokenOfString XP.tokenStrings
        fileName        = "foo"
   in   XP.runWrapParserG tokenTable
                stringOfToken posOfToken tokenOfString 
                fileName XP.pExp toks
