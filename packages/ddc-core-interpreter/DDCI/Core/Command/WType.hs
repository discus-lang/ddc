
module DDCI.Core.Command.WType
        (cmdShowWType)
where
import DDCI.Core.Token
import DDC.Core.Pretty
import DDC.Core.Exp
import DDC.Core.Check
import qualified DDC.Core.Parser.Lexer  as CP
import qualified DDC.Core.Parser.Tokens as CP
import qualified DDC.Core.Parser        as CP
import Control.Monad


-- | Show the type of a witness.
cmdShowWType :: String -> IO ()
cmdShowWType ss
 = case sequence (CP.lexExp ss) of
        Nothing         -> putStrLn "lexical error"
        Just toks       -> showWType_toks $ map Token toks


showWType_toks :: [Token] -> IO ()
showWType_toks toks                
 = case parseWitness toks of 
        Left err        -> putStrLn $ "parse error " ++ show err
        Right w         -> showWType_type w

        
showWType_type :: Witness Token -> IO ()
showWType_type w
 = case typeOfWitness w of
        Left err        -> putStrLn $ show $ ppr err
        Right k         -> putStrLn $ show $ (ppr w <> text " :: " <> ppr k)


parseWitness :: [Token] -> Either CP.ParseError (Witness Token)
parseWitness toks
 = let  tokenTable      = CP.liftTokens stringOfToken tokenOfString CP.tokenStrings
        fileName        = "foo"
   in   CP.runWrapParserG tokenTable
                stringOfToken posOfToken tokenOfString 
                fileName CP.pWitness toks
