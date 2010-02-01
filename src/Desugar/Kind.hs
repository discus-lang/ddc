
module Desugar.Kind
	( inferKindsTree
	, Constraint(..)
	, KindSource(..))

where

import Desugar.Plate.Trans
import Desugar.Data
import Desugar.Exp

import Type.Util.Elaborate
import Type.Util.Kind
import Type.Util.Bits
import Type.Exp
import Source.Error
import Shared.Pretty
import Shared.Error
import Shared.Base
import Shared.Var		(Var, NameSpace(..))
import Shared.VarPrim
import qualified Shared.Var	as Var
import qualified Type.Plate.Trans	as T

import Util
import Data.Set			(Set)
import Data.Map			(Map)
import Data.Sequence		as Seq
import Control.Monad.State.Strict
import qualified Data.Set	as Set
import qualified Data.Map	as Map
import qualified Data.Foldable	as Foldable
import qualified Debug.Trace

-----
stage	= "Desugar.Kind"
{-
debug	= True
trace ss xx
 = if debug 
 	then Debug.Trace.trace (pprStrPlain ss) xx
	else xx
-}

-- Types -------------------------------------------------------------------------------------------
-- | A Kind constraint
data Constraint 
	= Constraint KindSource Var Kind
	deriving (Show)
	
instance Pretty Constraint PMode where
 ppr (Constraint ks v k)	
 	= padL 20 v <> "::" <> padL 40 k <> parens ks % ";\n"
	
-- | Carries information about where a kind constraint came from.
data KindSource
	-- | Kind from the head of a data type definition
	= KSData SourcePos	

	-- | Kind from an effect definition
	| KSEffect SourcePos

	-- | Kind from a class definition
	| KSClass  SourcePos

	-- | Kind from a kind signature
	| KSSig  SourcePos
	deriving (Show)

instance Pretty KindSource PMode where
 ppr ks	= ppr $ show ks	


----------------------------------------------------------------------------------------------------
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


elabDataP :: Top SourcePos -> SolveM (Top SourcePos)
elabDataP pp
 = case pp of
 	PData{}	
	 -> do	pp'@(PData sp v vs ctors)	
			<- elaborateData newVarN getKind pp

		return	pp'
		
	PTypeSynonym{}	
	 -> do	pp'	<- elaborateTypeSynonym newVarN getKind pp
		return	pp'

	_	-> return pp


-- Tag each data constructor with its kind from this table
tagKindsTree :: Tree SourcePos -> SolveM (Tree SourcePos)
tagKindsTree pp
	= mapM (transZM (transTableId return)
		{ transT	= T.transformTM tagKindsT })
		pp
		
tagKindsT :: Type -> SolveM Type
tagKindsT tt
 	| TVar k v	<- tt
	= do	kindMap	<- gets stateKinds 
		case Map.lookup v kindMap of
			Nothing	-> return $ tt
			Just k'	-> return $ TVar k' v
		
	| Just (v, k, ts)	<- takeTData tt
	= do	kindMap	<- gets stateKinds
		case Map.lookup v kindMap of
			Nothing	-> return tt
			Just k'	-> return $ makeTData v k' ts
		
	| otherwise
	= return tt

-- | Elaborate regions in 
elabRegionsTree :: Tree SourcePos -> SolveM (Tree SourcePos)
elabRegionsTree pp
	= mapM (transZM (transTableId return)
		{ transP	= elabRegionsP
		, transS_leave	= elabRegionsS 
		, transX_leave	= elabRegionsX })
		pp

elabRegionsP pp
 = case pp of
	PExtern sp v t ot
	 -> do	t'	<- elabRegionsT t
		return	$ PExtern sp v t' ot
		
	PClassDict sp v ts context vts
	 -> do	ts'	<- mapM elabRegionsT ts
		let (vs, mts)	= unzip vts
		mts'	<- mapM elabRegionsT mts
		return	$ PClassDict sp v ts' context (Util.zip vs mts')
		
	PClassInst sp v ts context ss
	 -> do	ts'	<- mapM elabRegionsT ts
		return	$ PClassInst sp v ts' context ss
	
	PProjDict sp t ss
	 -> do	t'	<- elabRegionsT t
		return	$ PProjDict sp t' ss
	
	PSig sp v t
	 -> do	t'	<- elabRegionsT t
		return	$ PSig sp v t'
			
	_ ->	return pp

elabRegionsS ss
 = case ss of
	SSig sp v t
	 -> do	t'	<- elabRegionsT t
		return	$ SSig sp v t'

	_		-> return ss


elabRegionsX xx
 = case xx of
	XProjT sp t j
	 -> do	t'	<- elabRegionsT t
		return	$ XProjT sp t' j
	
	_ ->	return xx

elabRegionsT t
 = do	(t_elab, _)	<- elaborateRsT newVarN t
   	return t_elab


-- Slurp -------------------------------------------------------------------------------------------

-- | Slurp kind constraints from the desugared module
slurpConstraints :: Tree SourcePos -> Seq Constraint
slurpConstraints ps
	= Seq.fromList $ catMap slurpConstraint ps
	
slurpConstraint pp
 = case pp of
 	PTypeKind sp v k	
 	 -> [Constraint (KSSig sp) v k]

	PClassDict sp v ts context vts
	 -> map (\(TVar k v) -> Constraint (KSClass sp) v (defaultKind v k)) ts

 	PData sp v vs ctors	
	 -> let	k	= makeDataKind vs
	        k'	= forcePrimaryRegion v k
	    in	[Constraint (KSData sp) v k']

	PExternData sp name v k
	 -> [Constraint (KSData sp) v k]

	PEffect sp v k	-> [Constraint (KSEffect sp) v k]
--	PClass sp v k	-> [ConstraintS (KSClass sp) v k]
	_		-> []


defaultKind v k
 	| k == KNil	= kindOfSpace $ Var.nameSpace v
	| otherwise	= k 


-- Make sure the kinds of data type constructors have their primary regions.
forcePrimaryRegion :: Var -> Kind -> Kind
forcePrimaryRegion vData k

	-- unit doesn't need one
 	| vData == primTUnit
	= k

	-- these abstract types don't need one
	| elem vData [primTObj, primTData, primTThunk]
	= k

	-- unboxed data types don't need one
	| varIsUnboxedTyConData vData
	= k

	-- don't elaborate types with higher kinds
	| KFun kR _	<- k
	, kR	== kRegion
	= k
	
	| otherwise
	= KFun kRegion k


-- State -------------------------------------------------------------------------------------------

data SolveS
	= StateS 
	{ stateVarGen	:: Var.VarBind
	, stateKinds	:: Map Var Kind  }

stateInit unique
	= StateS
	{ stateVarGen	= Var.XBind unique 0
	, stateKinds	= Map.empty }
	
type SolveM = State SolveS


-- | Create a fresh variable
newVarN :: NameSpace -> SolveM Var
newVarN space
 = do	varId@(Var.XBind p i)	<- gets stateVarGen
 
	let name	= "r" ++ p ++ show i
	let var		= (Var.new name) 
			{ Var.bind 	= varId
			, Var.nameSpace = space }
	
	modify $ \s -> s { stateVarGen = Var.XBind p (i + 1) }
	
	return var

-- | Get the kind of a variable
getKind :: Var -> SolveM Kind
getKind v
 = do	kindMap	<- gets stateKinds
 	case Map.lookup v kindMap of
	 Just k		-> return k
	 Nothing	-> panic stage
	 		$ "getKind: no kind for " % v % "\n"

-- | Solve these kind constraints
solveConstraints :: Seq Constraint -> SolveM ()
solveConstraints constraints
 = do	Foldable.mapM_ addConstraint constraints
	return ()
 	

-- | Add a contraint to the state
addConstraint :: Constraint -> SolveM ()
addConstraint (Constraint src v k)
 = do	state	<- get

 	case Map.lookup v (stateKinds state) of
	 Nothing	
	  -> do	let state'	= state { stateKinds = Map.insert v k (stateKinds state) }
	  	put state'
		return	()
		
	 Just k'
	  -> addConstraint_unify v k k'
	 
addConstraint_unify v k k'
	| k == k'
	= return ()
	
	| otherwise
	= panic stage
	$ "addConstraint_unify: can't unify kinds for" <> v <> parens k <> parens k'






	
