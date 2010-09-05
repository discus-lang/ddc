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
import Source.Error
import Desugar.Plate.Trans
import DDC.Desugar.Elaborate.EffClo
import DDC.Desugar.Elaborate.Constraint
import DDC.Desugar.Elaborate.Regions
import DDC.Desugar.Elaborate.Slurp
import DDC.Desugar.Elaborate.State
import DDC.Desugar.Exp
import DDC.Type.Data.Elaborate
import DDC.Type.Data
import DDC.Type
import DDC.Var
import DDC.Base.SourcePos
import Control.Monad.State.Strict
import Data.Sequence			(Seq)
import Data.Map				(Map)
import qualified DDC.Type.Transform	as T
import qualified Data.Sequence		as Seq
import qualified Data.Map		as Map


-- | Elaborate types in this tree.
elaborateTree 
	:: String		-- ^ Unique
	-> Tree SourcePos	-- ^ Header tree
	-> Tree SourcePos	-- ^ Module tree
	-> ( Tree SourcePos 	-- new header tree
	   , Tree SourcePos	-- new module tree
	   , Seq Constraint	-- the kind constaints
	   , Map Var Kind	-- the kind of every type constructor
	   , [Error])		-- errors found during elaboration

elaborateTree unique psHeader psModule
 = let
	((psHeader', psModule', constraints), state')
		= runState
			(elaborateTreeM psHeader psModule)
			(stateInit unique)
	
   in	( psHeader'
	, psModule'
	, constraints
	, stateKinds state'
	, [] )
		
elaborateTreeM psHeader psModule
 = do	
	-- Slurp out kind constraints from the program
 	let constraints	=      slurpConstraints psHeader
			Seq.>< slurpConstraints psModule

	-- Solve kind constraints
	solveConstraints constraints

	-- Update the kinds of all the type consructors
	-- TODO: huh? shouldn't this come after the next step?
	psHeader_tagged	<- tagKindsTree psHeader
	psModule_tagged	<- tagKindsTree psModule

	-- Elaborate data type definitions
	-- TODO: this probably needs to be merged with kind inference when it's ready
	psHeader_data	<- mapM elabDataP psHeader_tagged
	psModule_data	<- mapM elabDataP psModule_tagged
	
	-- Now that we know what the kinds of all the type constructors are,
	-- add missing reigon variables to type signatures.
	psHeader_rs	<- elabRegionsTree psHeader_data
	psModule_rs	<- elabRegionsTree psModule_data

	-- Elaborate effects and closures
	psHeader_effclo	<- mapM elaborateEffCloP psHeader_rs
	psModule_effclo	<- mapM elaborateEffCloP psModule_rs
	
	return	( psHeader_effclo
		, psModule_effclo
		, constraints)

-- Data -------------------------------------------------------------------------------------------
-- | Elaborate data type definitions in this tree
elabDataP :: Top SourcePos -> ElabM (Top SourcePos)
elabDataP pp
 = case pp of
	PData sp dataDef@(DataDef { dataDefSeaName = Nothing })
	 -> do	dataDef'	<- elaborateDataDef newVarN dataDef
		return		$ PData sp dataDef'
		
	_ -> return pp


-- Tag Kinds --------------------------------------------------------------------------------------
-- Tag each data constructor with its kind from this table
tagKindsTree :: Tree SourcePos -> ElabM (Tree SourcePos)
tagKindsTree pp
	= mapM (transZM (transTableId return)
		{ transT	= T.transformTM tagKindsT })
		pp
		
tagKindsT :: Type -> ElabM Type
tagKindsT tt
 	| TVar _ (UVar v)	<- tt
	= do	kindMap	<- gets stateKinds 
		case Map.lookup v kindMap of
			Nothing	-> return $ tt
			Just k'	-> return $ TVar k' $ UVar v
		
	| Just (v, _, ts)	<- takeTData tt
	= do	kindMap	<- gets stateKinds
		case Map.lookup v kindMap of
			Nothing	-> return tt
			Just k'	-> return $ makeTData v k' ts
		
	| otherwise
	= return tt


-- EffClo -----------------------------------------------------------------------------------------
-- | Elaborate effects and closures in a top level signature.
elaborateEffCloP :: Top SourcePos -> ElabM (Top SourcePos)
elaborateEffCloP pp
  = case pp of
	PExtern sp v t mt
	 -> do	t'	<- elaborateEffCloInFunSigT t
		return	$ PExtern sp v t' mt
		
	PTypeSig a vs t
	 -> do	t'	<- elaborateEffCloInFunSigT t
		return	$ PTypeSig a vs t'
	
	_ -> return pp


