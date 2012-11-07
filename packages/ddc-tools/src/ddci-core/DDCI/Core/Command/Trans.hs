{-# OPTIONS -Werror #-}

module DDCI.Core.Command.Trans
        ( cmdTrans
	, cmdTransEval
	, applyTransAndCheck)
where
import DDCI.Core.Command.Eval
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Driver.Command.Check
import DDC.Build.Language
import DDC.Core.Fragment.Profile
import DDC.Core.Transform.Reannotate
import DDC.Core.Simplifier
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Equiv
import DDC.Type.Subsumes
import DDC.Base.Pretty
import DDC.Type.Env                             as Env
import qualified Control.Monad.State.Strict     as S
import qualified DDC.Core.Eval.Name             as Eval
import qualified Data.Set               as Set
import Data.Typeable
import Control.Monad
import DDC.Core.Module


-- Trans ----------------------------------------------------------------------
-- | Apply the current transform to an expression.
cmdTrans :: State -> Source -> String -> IO ()
cmdTrans state source str
 | Bundle fragment modules zero simpl _     <- stateBundle state
 , Fragment profile _ _ _ _ _ _ _ _ <- fragment
 =   cmdParseCheckExp fragment modules True source str 
 >>= goStore profile modules zero simpl
 where
        -- Expression had a parse or type error.
        goStore _ _ _ _ Nothing
         = do   return ()

        -- Expression is well-typed.
        goStore profile modules zero simpl (Just x)
         = do   let kenv    = modulesExportKinds modules (profilePrimKinds profile)
                let tenv    = modulesExportTypes modules (profilePrimTypes profile)

                tr <- applyTransAndCheck state profile kenv tenv zero simpl x
		case tr of
		  Nothing -> return ()
		  Just x' -> outDocLn state $ ppr x'


-- TransEval ------------------------------------------------------------------
-- | Apply the current transform to an expression,
--   then evaluate and display the result
cmdTransEval :: State -> Source -> String -> IO ()
cmdTransEval state source str
 | Bundle fragment modules0 zero simpl0 _	<- stateBundle state
 , Fragment profile0 _ _ _ _ _ _ _ _		<- fragment

 -- The evaluator only works on expressions with Eval.Names, 
 --   The actual name type is an existential of Bundle, 
 --   so we use gcast to check whether the bundle is really
 --   set to Eval.
 , Just (profile :: Profile Eval.Name) <- gcast profile0 
 , Just (SimplBox simpl)               <- gcast (SimplBox simpl0)
 , Just (modules :: ModuleMap (AnTEC () Eval.Name) Eval.Name)
				       <- gcast modules0
 = do   result  <- cmdParseCheckExp fragmentEval modules False source str 
        case result of
         Nothing         -> return ()
         Just xx
          -> do let kenv    = modulesExportKinds modules (profilePrimKinds profile)
                let tenv    = modulesExportTypes modules (profilePrimTypes profile)

                -- Apply the current transform.
                tr      <- applyTransAndCheck state profile kenv tenv zero simpl xx

                case tr of
                 Nothing -> return ()
                 Just x'
                     -- Evaluate the transformed expression.
                  -> do outDocLn state $ ppr x'
                        evalExp state x'
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
applyTransAndCheck 
        :: (Eq n, Ord n, Pretty n, Show n, Show a)
        => State
        -> Profile n
        -> Env n                        -- Kind Environment.
        -> Env n                        -- Type Environment.
        -> s
        -> Simplifier s (AnTEC a n) n
        -> Exp (AnTEC a n) n
        -> IO (Maybe (Exp (AnTEC () n) n))

applyTransAndCheck state profile kenv tenv zero simpl xx
 = do
        let Just annot  = takeAnnotOfExp xx
        let t1          = annotType    annot
        let eff1        = annotEffect  annot
        let clo1        = annotClosure annot

         -- Apply the simplifier.
        let tx          = flip S.evalState zero
                        $ applySimplifierX profile kenv tenv simpl xx
	
	let x'          = reannotate (const ()) $ result tx

	when (Set.member TraceTrans $ stateModes state)
	 $ case (resultInfo tx) of
	   TransformInfo inf
	    -> outDocLn state (text "* TRANSFORM INFORMATION: " <$> indent 4 (ppr inf) <$> text "")

        -- Check that the simplifier perserved the type of the expression.
        case checkExp (configOfProfile profile) kenv tenv x' of
          Right (x2, t2, eff2, clo2)
           |  equivT t1 t2
           ,  subsumesT kEffect  eff1 eff2
           ,  subsumesT kClosure clo1 clo2
           -> do return (Just x2)

           | otherwise
           -> do putStrLn $ renderIndent $ vcat
                    [ text "* CRASH AND BURN: Transform is not type preserving."
                    , ppr x'
                    , text ":: 1 " <+> ppr t1
                    , text ":: 2 " <+> ppr t2
                    , text ":!:1 " <+> ppr eff1
                    , text ":!:2 " <+> ppr eff2
                    , text ":$:1 " <+> ppr clo1
                    , text ":$:2 " <+> ppr clo2 ]
                 return Nothing

          Left err
           -> do putStrLn $ renderIndent $ vcat
                    [ text "* CRASH AND BURN: Type error in transformed program."
                    , ppr err
                    , text "" ]

                 outDocLn state $ text "Transformed expression:"
                 outDocLn state $ ppr x'
                 return Nothing

