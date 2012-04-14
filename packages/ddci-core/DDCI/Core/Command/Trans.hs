

module DDCI.Core.Command.Trans
        ( cmdTrans
	, cmdTransEval)
where
import DDCI.Core.Language
import DDCI.Core.Command.Check
import DDCI.Core.Command.Eval
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Core.Eval.Env
import DDC.Core.Eval.Name
import DDC.Core.Simplifier
import DDC.Core.Collect
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Equiv
import DDC.Type.Subsumes
import DDC.Base.Pretty
import qualified Control.Monad.State.Strict     as S
import qualified DDC.Type.Env                   as Env


-- | Apply the current transform to an expression.
cmdTrans :: State -> Source -> String -> IO ()
cmdTrans state source str
 = cmdParseCheckExp state fragmentEval source str >>= goStore
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore (Just (x, t1, eff1, clo1))
         = do   tr <- applyTrans state (x, t1, eff1, clo1)
		case tr of
		  Nothing -> return ()
		  Just x' -> outDocLn state $ ppr x'


-- | Transform an expression, or display errors
applyTrans 
        :: State 
        -> (Exp () Name, Type Name, Effect Name, Closure Name) 
        -> IO (Maybe (Exp () Name))

applyTrans state (x, t1, eff1, clo1)
 | Fragment _ _ _ _ _ makeNamifierT makeNamifierX nameZero <- fragmentEval
 = do	-- Collect names already used as binders. 
        -- We won't return these when asked for a fresh name.
        let (tbinds, vbinds) = collectBinds x
        let kenv        = Env.fromList tbinds
        let tenv        = Env.fromList vbinds

        -- Apply the simplifier.
        let x' = flip S.evalState nameZero
                $ applySimplifierX 
                        (stateSimplifier state) 
                        (stateRewriteRulesList state)
                        (makeNamifierT kenv) (makeNamifierX tenv)
                        x

        -- Check that the simplifier perserved the type of the expression.
	case checkExp primDataDefs primKindEnv primTypeEnv x' of
	  Right (_, t2, eff2, clo2)
	   |  equivT t1 t2
	   ,  subsumesT kEffect  eff1 eff2
	   ,  subsumesT kClosure clo1 clo2
	   -> do return (Just x')

	   | otherwise
	   -> do putStrLn $ renderIndent $ vcat
		    [ text "* CRASH AND BURN: Transform is not type preserving."
		    , ppr x'
		    , text "::"  <+> ppr t2
		    , text ":!:" <+> ppr eff2
		    , text ":$:" <+> ppr clo2 ]
		 return Nothing

	  Left err
	   -> do putStrLn $ renderIndent $ vcat
		    [ text "* CRASH AND BURN: Type error in transformed program."
		    , ppr err
		    , text "" ]

		 outDocLn state $ text "Transformed expression:"
		 outDocLn state $ ppr x'
		 return Nothing


-- | Apply the current transform to an expression,
--   then evaluate and display the result
cmdTransEval :: State -> Source -> String -> IO ()
cmdTransEval state source str
 = cmdParseCheckExp state fragmentEval source str >>= goStore
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
		   -> do outDocLn state $ ppr x'
			 evalExp state (x',t1,eff1,clo1)
                         return ()
