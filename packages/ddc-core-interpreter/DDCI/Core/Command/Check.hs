module DDCI.Core.Command.Check
        ( cmdShowKind
        , cmdShowWType
        , cmdShowType
        , ShowTypeMode(..))
where
import DDCI.Core.Prim.Env
import DDCI.Core.Prim.Name
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Parser.Lexer
import DDC.Core.Parser
import qualified DDC.Type.Parser        as T
import qualified DDC.Type.Check         as T
import qualified DDC.Core.Transform     as C
import qualified DDC.Base.Parser        as BP


-- kind -------------------------------------------------------------------------------------------
cmdShowKind :: String -> IO ()
cmdShowKind ss
 = goParse (lexExp Name ss)
 where
        goParse toks                
         = case BP.runTokenParser show "<interactive>" T.pType toks of 
                Left err        -> putStrLn $ "parse error " ++ show err
                Right t         -> goCheck t

        goCheck t
         = case T.checkType primEnv (C.spread primEnv t) of
                Left err        -> putStrLn $ show $ ppr err
                Right k         -> putStrLn $ show $ (ppr t <> text " :: " <> ppr k)



-- wtype ------------------------------------------------------------------------------------------
-- | Show the type of a witness.
cmdShowWType :: String -> IO ()
cmdShowWType ss
 = goParse (lexExp Name ss)
 where
        goParse toks
         = case BP.runTokenParser show "<interactive>" pWitness toks of 
                Left err        -> putStrLn $ "parse error " ++ show err
                Right w         -> goCheck w

        goCheck w 
         = case typeOfWitness w of
                Left err        -> putStrLn $ show $ ppr (err :: Error () () Name)
                Right k         -> putStrLn $ show $ (ppr w <> text " :: " <> ppr k)


-- check / type / effect / closure ----------------------------------------------------------------
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
 = goParse (lexExp Name ss)
 where
        goParse toks                
         = case BP.runTokenParser 
                        show "<interactive>"
                        (pExp (PrimHandler makePrimLiteral))
                        toks 
            of  Left err -> putStrLn $ "parse error " ++ show err
                Right x  
                 -> let x'  = C.spread primEnv x
                    in  goCheck x' (checkExp typeOfPrim primEnv x')

        
        goCheck _ (Left err)
         = putStrLn $ show $ ppr err

        goCheck x (Right (t, eff, clo))
         = case mode of
                ShowTypeAll
                 -> putStrLn $ show 
                 $ vcat [ ppr x
                        , nest 4 $ text ":: " <> ppr t
                        , nest 4 $ text ":!: " <> ppr eff
                        , nest 4 $ text ":$: " <> ppr clo]
        
                ShowTypeValue
                 -> putStrLn $ pretty 100 (ppr x <> text " :: " <> ppr t)
        
                ShowTypeEffect
                 -> putStrLn $ pretty 100 (ppr x <> text " :! " <> ppr eff)

                ShowTypeClosure
                 -> putStrLn $ pretty 100 (ppr x <> text " :$ " <> ppr clo)
