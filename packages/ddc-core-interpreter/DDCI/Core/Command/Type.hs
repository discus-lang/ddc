{-# OPTIONS -fno-warn-missing-signatures #-}
module DDCI.Core.Command.Type
        ( cmdShowType
        , ShowTypeMode(..))
where
import DDCI.Core.Prim.Name
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import DDC.Core.Parser.Tokens
import DDC.Core.Parser
import DDC.Base.Lexer
import qualified DDC.Base.Parser        as BP
import qualified DDC.Type.Check.Env     as Env


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
        = goParse mode (lexExp Name ss)

goParse mode toks                
 = case parseExp toks of 
    Left err -> putStrLn $ "parse error " ++ show err
    Right x  -> goCheck mode x (checkExp Env.empty x)

        
goCheck _ _ (Left err)
        = putStrLn $ show $ ppr err

goCheck mode x (Right (t, eff, clo))
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


parseExp :: [Token (Tok Name)] -> Either BP.ParseError (Exp () p Name)
parseExp toks
        = BP.runTokenParser show "<interactive>" pExp toks

