
module DDC.Driver.Command.Ast
        ( cmdAstModule
        , cmdAstExp)
where
import DDC.Driver.Command.Check
import DDC.Driver.Source
import DDC.Build.Language
import Control.Monad.Trans.Error
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Pretty as H


-- | Parse, check, and pretty print a module's internal representation.
cmdAstModule :: Language -> Source -> String -> IO ()
cmdAstModule language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 = do   mModule <- runErrorT 
                $ cmdCheckModuleFromString fragment source str
        goShow mModule
 where
        -- Expression had a parse or type error.
        goShow (Left err)
         = do   putStrLn err
                return ()

        -- Expression is well-typed.
        goShow (Right x)
         = let p = pretty x in
           putStrLn p
         
        pretty x
         = case H.parseExp (show x) of
                H.ParseOk parsed -> H.prettyPrint parsed
                err -> show err


-- | Parse, check, and pretty print an expression's internal representation.
cmdAstExp :: Language -> Source -> String -> IO ()
cmdAstExp language source str
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 , modules              <- bundleModules  bundle
 =   cmdParseCheckExp fragment modules True source str 
 >>= goShow
 where
        -- Expression had a parse or type error.
        goShow Nothing
         = return ()

        -- Expression is well-typed.
        goShow (Just x)
         = let p = pretty x in
	   putStrLn p
	 
	pretty x
	 = case H.parseExp (show x) of
		H.ParseOk parsed -> H.prettyPrint parsed
		err -> show err

