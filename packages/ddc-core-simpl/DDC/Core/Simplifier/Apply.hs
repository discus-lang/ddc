
-- | Application of simplifiers to modules and expressions.
module DDC.Core.Simplifier.Apply
        ( applySimplifier
        , applyTransform

        , applySimplifierX
        , applyTransformX)
where
import DDC.Base.Pretty
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Fragment
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.Snip
import DDC.Core.Transform.Flatten
import DDC.Core.Transform.Beta
import DDC.Core.Transform.DeadCode
import DDC.Core.Transform.Forward
import DDC.Core.Transform.Bubble
import DDC.Core.Transform.Inline
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Rewrite
import DDC.Core.Transform.Elaborate
import DDC.Type.Env                     (Env)
import Control.Monad.State.Strict
import qualified DDC.Base.Pretty	as P
import Data.Typeable (Typeable)


-- Modules --------------------------------------------------------------------
-- | Apply a simplifier to a module.
--
--   The state monad holds a fresh name generator.
applySimplifier 
        :: (Show a, Ord n, Show n, Pretty n) 
	=> Profile n		-- ^ Profile of language we're working in
	-> Env n		-- ^ Kind environment
	-> Env n		-- ^ Type environment
        -> Simplifier s a n     -- ^ Simplifier to apply.
        -> Module a n           -- ^ Module to simplify.
        -> State s (Module a n)

applySimplifier profile kenv tenv spec mm
 = case spec of
        Seq t1 t2
         -> do  mm'     <- applySimplifier profile kenv tenv t1 mm
                applySimplifier profile kenv tenv t2 mm'

        Trans t1
         -> applyTransform profile kenv tenv t1 mm
	
	-- finish fix: applyTransform should really return a TransformResult
	Fix 0 _
	 -> return mm
	Fix n s
	 -> do	mm' <- applySimplifier profile kenv tenv s mm
		applySimplifier profile kenv tenv (Fix (n-1) s) mm'


-- | Apply a transform to a module.
applyTransform
        :: (Show a, Ord n, Show n, Pretty n)
	=> Profile n		-- ^ Profile of language we're working in
	-> Env n		-- ^ Kind environment
	-> Env n		-- ^ Type environment
        -> Transform s a n      -- ^ Transform to apply.
        -> Module a n           -- ^ Module to simplify.
        -> State s (Module a n)

applyTransform profile kenv tenv spec mm
 = case spec of
        Id               -> return mm
        Anonymize        -> return $ anonymizeX mm
        Snip             -> return $ snip mm
        Flatten          -> return $ flatten mm
        Beta             -> return $ result $ betaReduce False mm
        BetaLets         -> return $ result $ betaReduce True  mm
        Forward          -> return $ forwardModule mm
        Bubble           -> return $ bubbleModule mm
        Namify namK namT -> namifyUnique namK namT mm
        Inline getDef    -> return $ inline getDef mm
        Rewrite rules    -> return $ rewriteModule rules mm
        DeadCode         -> return $ deadCodeModule profile kenv tenv mm
        Elaborate        -> return $ elaborateModule mm


-- Expressions ----------------------------------------------------------------
-- | Apply a simplifier to an expression.
--
--   The state monad holds a fresh name generator.
applySimplifierX 
        :: (Show a, Show n, Ord n, Pretty n)
	=> Profile n		-- ^ Profile of language we're working in
	-> Env n		-- ^ Kind environment
	-> Env n		-- ^ Type environment
        -> Simplifier s a n     -- ^ Simplifier to apply.
        -> Exp a n              -- ^ Exp to simplify.
        -> State s (TransformResult (Exp a n))

applySimplifierX profile kenv tenv spec xx
 = let down = applySimplifierX profile kenv tenv
   in  case spec of
        Seq t1 t2
         -> do  tx  <- down t1 xx
                tx' <- down t2 (result tx)

		let info =
			case (resultInfo tx, resultInfo tx') of
			(TransformInfo i1, TransformInfo i2) -> SeqInfo i1 i2
		
                let again    = resultAgain    tx || resultAgain    tx'
                let progress = resultProgress tx || resultProgress tx'

		return TransformResult
		    { result   	     = result tx'
                    , resultAgain    = again
		    , resultProgress = progress
		    , resultInfo     = TransformInfo info }

	Fix i s
	 -> do	tx <- applyFixpointX profile kenv tenv i s xx
		let info =
			case resultInfo tx of
			TransformInfo info1 -> FixInfo i info1
		
		return TransformResult
		    { result   	     = result tx
                    , resultAgain    = resultAgain    tx
		    , resultProgress = resultProgress tx
		    , resultInfo     = TransformInfo info }
		
        Trans t1
         -> applyTransformX profile kenv tenv t1 xx


-- | Apply a simplifier until it stops progressing, or a maximum number of times
applyFixpointX
        :: (Show a, Show n, Ord n, Pretty n)
	=> Profile n		-- ^ Profile of language we're working in
	-> Env n		-- ^ Kind environment
	-> Env n		-- ^ Type environment
        -> Int			-- ^ Maximum number of times to apply
	-> Simplifier s a n     -- ^ Simplifier to apply.
        -> Exp a n              -- ^ Exp to simplify.
        -> State s (TransformResult (Exp a n))

applyFixpointX profile kenv tenv i' s xx'
 = go i' xx' False
 where
  simp = applySimplifierX profile kenv tenv s

  go 0 xx progress = do
    tx <- simp xx
    return tx { resultProgress = progress }

  go i xx progress = do
    tx <- simp xx
    case resultAgain tx of
	False 
         -> return tx { resultProgress = progress }

	True  
         -> do
	    tx' <- go (i-1) (result tx) True
	    let info =
		    case (resultInfo tx, resultInfo tx') of
		    (TransformInfo i1, TransformInfo i2) -> SeqInfo i1 i2
	    
	    return TransformResult
		{ result   	 = result tx'
                , resultAgain    = resultProgress tx'
		, resultProgress = resultProgress tx'
		, resultInfo     = TransformInfo info }


-- | Result of applying two simplifiers in sequence.
data SeqInfo
        = forall i1 i2
        . (Typeable i1, Typeable i2, Pretty i1, Pretty i2)
        => SeqInfo i1 i2
        deriving Typeable


instance Pretty SeqInfo where
 ppr (SeqInfo i1 i2) = ppr i1 P.<> text ";" <$> ppr i2


-- | Result of applying a simplifier until we reach a fixpoint.
data FixInfo
        = forall i1
        . (Typeable i1, Pretty i1)
        => FixInfo Int i1
        deriving Typeable


instance Pretty FixInfo where
 ppr (FixInfo num i1) 
  =  text "fix" <+> int num P.<> text ":"
  <$> indent 4 (ppr i1)


-- | Apply a single transform to an expression.
applyTransformX 
        :: (Show a, Show n, Ord n, Pretty n)
	=> Profile n		-- ^ Profile of language we're working in
	-> Env n		-- ^ Kind environment
	-> Env n		-- ^ Type environment
        -> Transform s a n      -- ^ Transform to apply.
        -> Exp a n              -- ^ Exp  to transform.
        -> State s (TransformResult (Exp a n))

applyTransformX profile kenv tenv spec xx
 = case spec of
        Id                -> res xx
        Anonymize         -> res    $ anonymizeX xx
        Snip              -> res    $ snip xx
        Flatten           -> res    $ flatten xx
        Inline  getDef    -> res    $ inline getDef xx
        Beta              -> return $ betaReduce False xx
        BetaLets          -> return $ betaReduce True  xx
        DeadCode          -> return $ deadCode profile kenv tenv xx
        Forward           -> return $ forwardX xx
        Bubble            -> res    $ bubbleX kenv tenv xx
        Namify  namK namT -> namifyUnique namK namT xx >>= res
        Rewrite rules     -> return $ rewrite rules xx
        Elaborate{}       -> res    $ elaborate [] xx
 where
    res x = return $ resultDone (show $ ppr spec) x
