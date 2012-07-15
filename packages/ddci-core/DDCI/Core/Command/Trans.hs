{-# OPTIONS -Werror #-}

module DDCI.Core.Command.Trans
        ( cmdTrans
	, cmdTransEval)
where
import DDCI.Core.Command.Check
-- import DDCI.Core.Command.Eval
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Build.Language
-- import DDC.Core.Eval.Env
-- import DDC.Core.Transform.Reannotate
import DDC.Core.Fragment.Profile
import DDC.Core.Simplifier
-- import DDC.Core.Collect
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Equiv
import DDC.Type.Subsumes
import DDC.Base.Pretty
import DDC.Core.Transform.Rewrite.Rule
import Data.Map                         (Map)
import qualified Control.Monad.State.Strict     as S
import qualified Data.Map                       as Map
import qualified DDC.Core.Eval.Name             as Eval
import Data.Typeable
-- import qualified DDC.Type.Env                   as Env


-- | Apply the current transform to an expression.
cmdTrans :: State -> Source -> String -> IO ()
cmdTrans state source str
 | Bundle fragment zero simpl rules       <- stateBundle state
 , Fragment profile _ _ _ _ _ _ _ _ <- fragment
 = cmdParseCheckExp state fragment True source str >>= goStore profile zero simpl rules
 where
        -- Expression had a parse or type error.
        goStore _ _ _ _ Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore profile zero simpl rules (Just (x, t1, eff1, clo1))
         = do   tr <- applyTrans state profile zero simpl rules (x, t1, eff1, clo1)
		case tr of
		  Nothing -> return ()
		  Just x' -> outDocLn state $ ppr x'


-- | Transform an expression, or display errors
applyTrans 
        :: (Eq n, Ord n, Pretty n, Show n, Show a)
        => State
        -> Profile n
        -> s
        -> Simplifier s a n
        -> Map String (RewriteRule a n)
        -> (Exp a n, Type n, Effect n, Closure n) 
        -> IO (Maybe (Exp a n))

applyTrans state profile zero simpl rules (x, t1, eff1, clo1)
 = do
         -- Apply the simplifier.
        let x' = flip S.evalState zero
               $ applySimplifierX simpl (Map.elems rules) x

        -- Check that the simplifier perserved the type of the expression.
	case checkExp (profilePrimDataDefs profile)
                      (profilePrimKinds profile)
                      (profilePrimTypes profile) x' of
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
cmdTransEval state _source _str
 | Bundle fragment zero simpl _     <- stateBundle state
 , Fragment profile _ _ _ _ _ _ _ _ <- fragment
 = case gcast profile of
        Just (profileEval :: Profile Eval.Name) -> error "yep"
        _                                       -> error "nope"

{-}
 = cmdParseCheckExp state fragmentEval False source str >>= goStore state profile zero simpl
 where
        -- Expression had a parse or type error.
        goStore _ _ _ _ Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore state profile s simpl (Just (x, t1, eff1, clo1))
         = do   tr <- applyTrans state profile s simpl (x, t1, eff1, clo1)
		case tr of
		  Nothing -> return ()
		  Just x'
		   -> do outDocLn state $ ppr x'
			 evalExp state (x',t1,eff1,clo1)
                         return ()
-}



