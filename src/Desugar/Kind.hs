
-- | Kind inference.
--   TODO: This isn't finished.
--
module Desugar.Kind
	( inferKindsTree
	, Constraint(..)
	, KindSource(..))
where
import Desugar.Plate.Trans
import DDC.Desugar.Exp
import Source.Error
import Util
import DDC.Base.SourcePos
import DDC.Type
import DDC.Type.Data
import DDC.Type.Data.Elaborate
import DDC.Desugar.Elaborate.Regions
import DDC.Desugar.Elaborate.Constraint
import DDC.Desugar.Elaborate.Slurp
import DDC.Desugar.Elaborate.State
import DDC.Var
import Data.Sequence			as Seq
import qualified DDC.Type.Transform	as T
import qualified Data.Map		as Map


-- | Infer the kinds for variables in this tree,
--	and fill in missing kind information on the variables.
inferKindsTree 
	:: String			-- unique
	-> Tree SourcePos		-- header tree
	-> Tree SourcePos		-- source tree
	-> ( Tree SourcePos		-- header tree with kinds filled in
	   , Tree SourcePos		-- source tree with kinds filled in
	   , Seq  Constraint		-- the kind constraints
	   , Map Var Kind		-- kind table for every type constructor
	   , [Error])			-- kind inference errors

inferKindsTree
	unique
	psHeader
	psSource
	
 = let	( (psHeader', psSource', constraints)
 	  , state')	= runState (inferKindsM psHeader psSource) (stateInit unique)

   in 	( psHeader'
 	, psSource'
	, constraints
	, stateKinds state'
	, [])

inferKindsM 
	psHeader 
	psSource
 = do
 	let constraints	=  slurpConstraints psHeader
			>< slurpConstraints psSource

	solveConstraints constraints

	psHeader_tag	<- tagKindsTree psHeader
	psSource_tag	<- tagKindsTree psSource

	psHeader_data	<- mapM elabDataP psHeader_tag
	psSource_data	<- mapM elabDataP psSource_tag

	psHeader_elab	<- elabRegionsTree psHeader_data
	psSource_elab	<- elabRegionsTree psSource_data

	return	( psHeader_elab
		, psSource_elab
		, constraints)


elabDataP :: Top SourcePos -> ElabM (Top SourcePos)
elabDataP pp
 = case pp of
	PData sp dataDef@(DataDef { dataDefSeaName = Nothing })
	 -> do	dataDef'	<- elaborateDataDef newVarN dataDef
		return		$ PData sp dataDef'
		
	_ -> return pp


-- Tag each data constructor with its kind from this table
tagKindsTree :: Tree SourcePos -> ElabM (Tree SourcePos)
tagKindsTree pp
	= mapM (transZM (transTableId return)
		{ transT	= T.transformTM tagKindsT })
		pp
		
tagKindsT :: Type -> ElabM Type
tagKindsT tt
 	| TVar k (UVar v)	<- tt
	= do	kindMap	<- gets stateKinds 
		case Map.lookup v kindMap of
			Nothing	-> return $ tt
			Just k'	-> return $ TVar k' $ UVar v
		
	| Just (v, k, ts)	<- takeTData tt
	= do	kindMap	<- gets stateKinds
		case Map.lookup v kindMap of
			Nothing	-> return tt
			Just k'	-> return $ makeTData v k' ts
		
	| otherwise
	= return tt




