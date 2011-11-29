
module DDCI.Core.Command.Type
        ( cmdShowType
        , ShowTypeMode(..))
where
import DDCI.Core.Token
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import qualified DDC.Type.Check.Env     as Env
import qualified DDC.Core.Parser.Lexer  as XP
import qualified DDC.Core.Parser.Tokens as XP
import qualified DDC.Core.Parser        as XP
import Control.Monad


-- | What components of the checked type to display.
data ShowTypeMode
        = ShowTypeAll
        | ShowTypeValue
        | ShowTypeEffect
        | ShowTypeClosure
        deriving (Eq, Show)


-- | Show the type of an expression.
cmdShowType :: ShowTypeMode -> String -> IO ()
cmdShowType mode ss
 = case sequence (XP.lexExp ss) of
        Nothing         -> putStrLn "lexical error"
        Just toks       -> showType_toks mode $ map Token toks


-- | Show the type of an expression,
--   given its tokens.
showType_toks :: ShowTypeMode -> [Token] -> IO ()
showType_toks mode toks                
 = case parseExp toks of 
    Left err -> putStrLn $ "parse error " ++ show err
    Right x  -> showType_exp mode x (checkExp Env.empty x)

        
-- | Show the type of an expresion
--   given the result of the type checking.
showType_exp
        :: (Eq n, Pretty n)
        => ShowTypeMode
        -> Exp a n p
        -> Either (Error a n p) (Type n, Effect n, Closure n)
        -> IO ()

showType_exp _ _ (Left err)
        = putStrLn $ show $ ppr err

showType_exp mode x (Right (t, eff, clo))
 = case mode of
        ShowTypeAll
         -> putStrLn $ show $ vcat 
                [ ppr x
                , nest 4 $ text ":: " <> ppr t
                , nest 4 $ text ":! " <> ppr eff
                , nest 4 $ text ":$ " <> ppr clo]
        
        ShowTypeValue
         -> putStrLn $ show (ppr x <> text " :: " <> ppr t)
        
        ShowTypeEffect
         -> putStrLn $ show (ppr x <> text " :! " <> ppr eff)

        ShowTypeClosure
         -> putStrLn $ show (ppr x <> text " :$ " <> ppr clo)


-- | Parse some tokens as an expression.
--
--   Use a fake file name in parser error messages because we assume the tokens
--   have just been read from the console.
parseExp :: [Token] -> Either XP.ParseError (Exp () Token p)
parseExp toks
 = let  tokenTable      = XP.liftTokens stringOfToken tokenOfString XP.tokenStrings
        fileName        = "<interactive>"
   in   XP.runWrapParserG tokenTable
                stringOfToken posOfToken tokenOfString 
                fileName XP.pExp toks

