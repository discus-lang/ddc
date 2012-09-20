
module DDC.Main.Command.Ast
        (cmdAst)
where
import DDC.Main.Command.Check
import DDC.Main.Source
import DDC.Main.Bundle
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Pretty as H


-- | Parse, check, and pretty print an expression's internal representation
cmdAst :: Bundle -> Source -> String -> IO ()
cmdAst bundle source str
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

