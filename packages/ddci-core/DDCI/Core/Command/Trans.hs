

module DDCI.Core.Command.Trans
        ( cmdTrans
	, cmdTransEval)
where
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Type.Equiv
import DDCI.Core.Transform
import DDCI.Core.State
import DDCI.Core.Command.Check
import DDCI.Core.Command.Eval
import DDCI.Core.Eval.Env
import DDCI.Core.Eval.Name
import DDC.Base.Pretty


-- | Apply the current transform to an expression.
cmdTrans :: State -> Int -> String -> IO ()
cmdTrans state lineStart str
 = cmdParseCheckExp lineStart str >>= goStore
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore (Just (x, t1, eff1, clo1))
         = do   tr <- applyTrans state (x, t1, eff1, clo1)
		case tr of
		  Nothing -> return ()
		  Just x' -> putStrLn (pretty $ ppr x')

                case checkExp primDataDefs Env.empty x' of
                  Right (_, t2, eff2, clo2)
                   |  equivT t1 t2
                   ,  equivT eff1 eff2
                   ,  equivT clo1 clo2
                   -> do putStrLn (pretty $ ppr x')
                         return ()

	   | otherwise
	   -> do putStrLn $ pretty $ vcat
		    [ text "* CRASH AND BURN: Transform is not type preserving."
		    , ppr x'
		    , text "::"  <+> ppr t2
		    , text ":!:" <+> ppr eff2
		    , text ":$:" <+> ppr clo2 ]
		 return Nothing

	  Left err
	   -> do putStrLn $ pretty $ vcat
		    [ text "* CRASH AND BURN: Type error in transformed program."
		    , ppr err
		    , text ""]

		 putStrLn "Transformed expression:"
		 
		 putStrLn (pretty $ ppr x')
		 return Nothing

-- | Apply the current transform to an expression, then evaluate and display the result
cmdTransEval :: State -> Int -> String -> IO ()
cmdTransEval state lineStart str
 = cmdParseCheckExp lineStart str >>= goStore
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore (Just (x, t1, eff1, clo1))
         = do   tr <- applyTrans state (x, t1, eff1, clo1)
		case tr of
		  Nothing -> return ()
		  Just x'
		   -> do putStrLn (pretty $ ppr x')
			 evalExp state (x',t1,eff1,clo1)
                         return ()

