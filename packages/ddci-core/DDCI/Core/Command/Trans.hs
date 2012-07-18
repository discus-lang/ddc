{-# OPTIONS -Werror #-}

module DDCI.Core.Command.Trans
        ( cmdTrans
	, cmdTransEval)
where
import DDCI.Core.Command.Check
import DDCI.Core.Command.Eval
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Core.Simplifier
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Equiv
import DDC.Type.Subsumes
import DDC.Base.Pretty
import DDC.Core.Transform.Reannotate
import qualified Control.Monad.State.Strict     as S
import qualified DDC.Core.Eval.Name             as Eval
import Data.Typeable


-- Trans ----------------------------------------------------------------------
-- | Apply the current transform to an expression.
cmdTrans :: State -> Source -> String -> IO ()
cmdTrans state source str
 | Bundle fragment _ zero simpl _     <- stateBundle state
 , Fragment profile _ _ _ _ _ _ _ _ <- fragment
 =   cmdParseCheckExp state fragment True source str 
 >>= goStore profile zero simpl
 where
        -- Expression had a parse or type error.
        goStore _ _ _ Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore profile zero simpl (Just (x, t1, eff1, clo1))
         = do   tr <- applyTrans state profile zero simpl (x, t1, eff1, clo1)
		case tr of
		  Nothing -> return ()
		  Just x' -> outDocLn state $ ppr x'


-- TransEval ------------------------------------------------------------------
-- | Apply the current transform to an expression,
--   then evaluate and display the result
cmdTransEval :: State -> Source -> String -> IO ()
cmdTransEval state source str
 | Bundle fragment _ zero simpl0 _       <- stateBundle state
 , Fragment profile0 _ _ _ _ _ _ _ _   <- fragment

 -- The evaluator only works on expressions with Eval.Names, 
 --   The actual name type is an existential of Bundle, 
 --   so we use gcast to check whether the bundle is really
 --   set to Eval.
 , Just (profile :: Profile Eval.Name) <- gcast profile0 
 , Just (SimplBox simpl)               <- gcast (SimplBox simpl0)
 = do   result  <- cmdParseCheckExp state fragmentEval False source str 
        case result of
         Nothing         -> return ()
         Just stuff@(_x, t1, eff1, clo1)
          -> do -- Apply the current transform.
                tr      <- applyTrans state profile zero simpl stuff
                case tr of
                 Nothing -> return ()
                 Just x'
                     -- Evaluate the transformed expression.
                  -> do outDocLn state $ ppr x'
                        let x'' = reannotate (const ()) x'
                        evalExp state (x'', t1, eff1, clo1)
                        return ()

 | otherwise
 = outDocLn state $ text "Language must be set to Eval for :teval"


-- Proxies to help with type casting.
--   The 'gcast' function will only cast the rightmost 'n' of a type,
--   But there is also an 'n' attached to the annotation type of
--   the simplifier. We use the proxy to cast both occurrences at once.
data SimplBox s n
        = SimplBox (Simplifier s (AnTEC () n) n)


-- Trans ----------------------------------------------------------------------
-- | Transform an expression, or display errors
applyTrans 
        :: (Eq n, Ord n, Pretty n, Show n, Show a)
        => State
        -> Profile n
        -> s
        -> Simplifier s a n
        -> (Exp a n, Type n, Effect n, Closure n) 
        -> IO (Maybe (Exp a n))

applyTrans state profile zero simpl (x, t1, eff1, clo1)
 = do
         -- Apply the simplifier.
        let x' = flip S.evalState zero
               $ applySimplifierX simpl x

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

