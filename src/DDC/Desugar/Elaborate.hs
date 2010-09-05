{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Elaborate data type definitions and type signatures in this tree.
--   In the source program we allow region, effect, and closure infomation to be elided
--   from data type definitions and type signatures.
-- 
--   For data type definitions, we add region effect and closure parameters using heuristics
--   based on how the data constructors are defined.
--
--   In type signatures we add fresh variables to data type constructor applications,
--   using the kind of the data type constructors as a guide. These varaiables are just 
--   place holders, don't constrain the type, and just turn into 'meta' variables during
--   type inference.
--
--   NOTE: At the moment this only elaborates the effect and closure information 
--         in type signatures. It runs after Desugar.Kinds which adds in missing region variables.

--   TODO: This is fairly ad-hoc at the moment, we'll need more experience with it to
--         determine if these heuristics are what we actually want. In all cases the 
--         program should work if you add in all the required type information manually.
--
--   TODO: I expect we'll want to combine kind inference with this process in the long run.
-- 
module DDC.Desugar.Elaborate 
	(elaborateTree)
where
import DDC.Desugar.Elaborate.EffClo
import DDC.Desugar.Elaborate.State
import DDC.Desugar.Exp
import DDC.Base.SourcePos
import Control.Monad.State.Strict


-- | Elaborate types in this tree.
elaborateTree 
	:: String		-- unique
	-> Tree SourcePos 
	-> Tree SourcePos 

elaborateTree unique tree
 = evalState (mapM elaborateP tree) (stateInit unique)


-- | Elaborate types in a top level thing.
elaborateP :: Top SourcePos -> ElabM (Top SourcePos)
elaborateP pp
  = case pp of
	PExtern sp v t mt
	 -> do	t'	<- elaborateEffCloInFunSigT t
		return	$ PExtern sp v t' mt
		
	PTypeSig a vs t
	 -> do	t'	<- elaborateEffCloInFunSigT t
		return	$ PTypeSig a vs t'
	
	_ -> return pp


