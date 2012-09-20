
module DDC.Driver.Command.Ast
        ( cmdAstModule
        , cmdAstExp)
where
import DDC.Driver.Command.Check
import DDC.Driver.Source
import DDC.Driver.Bundle
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Pretty as H


-- | Parse, check, and pretty print a module's internal representation.
cmdAstModule :: Bundle -> Source -> String -> IO ()
cmdAstModule bundle source str
 | Bundle frag _ _ _ _ <- bundle
 = cmdParseCheckModule frag source str >>= goShow
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


-- | Parse, check, and pretty print an expression's internal representation.
cmdAstExp :: Bundle -> Source -> String -> IO ()
cmdAstExp bundle source str
 | Bundle frag modules _ _ _ <- bundle
 = cmdParseCheckExp frag modules True source str >>= goShow
 where
        -- Expression had a parse or type error.
        goShow Nothing
         = return ()

        -- Expression is well-typed.
        goShow (Just (x, _tX, _effX, _cloX))
         = let p = pretty x in
	   putStrLn p
	 
	pretty x
	 = case H.parseExp (show x) of
		H.ParseOk parsed -> H.prettyPrint parsed
		err -> show err

