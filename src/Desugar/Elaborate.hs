
module Desugar.Elaborate 
	(elaborateTree)
where

import Type.Util.Elaborate	(elaborateEffT, elaborateCloT)
import Type.Util.Bits
import Type.Util.Kind
import Type.Plate
import Type.Pretty
import Type.Exp

import Desugar.Exp

import Shared.Var		(Var, NameSpace(..))
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import qualified Shared.VarUtil	as Var
import qualified Shared.VarPrim	as Var
import Shared.Pretty
import Shared.Base

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util

import qualified Debug.Trace

debug	= False
trace ss xx
 = if debug 
 	then Debug.Trace.trace (pprStrPlain ss) xx
	else xx

-----
elaborateTree 
	:: String		-- unique
	-> Tree SourcePos 
	-> Tree SourcePos 

elaborateTree unique tree
 = evalState (mapM elaborateP tree) (stateInit unique)

elaborateP :: Top SourcePos -> ElabM (Top SourcePos)
elaborateP pp
 = case pp of
	PExtern sp v t mt
	 -> do	t'	<- elaborateT t
		return	$ PExtern sp v t' mt
	
	_ -> return pp

elaborateT :: Type -> ElabM Type
elaborateT tt
 = case tt of
	TFun{}			-> elaborateT_fun tt
	TFetters fs (TFun{})	-> elaborateT_fun tt

	_	-> return tt

elaborateT_fun :: Type -> ElabM Type
elaborateT_fun tt 
 = do	let (tt', (rsRead, rsWrite))	
			= collectRsRW tt

	let free	= Set.filter (not . Var.isCtorName) $ freeVars tt'
	
	-- see what vars are already quantified in this scheme
	let quantVs
		= case tt' of
			TForall vks _	-> Set.fromList $ map fst vks
			_		-> Set.empty
	
	-- TODO: freeVars doesn't pass the kinds of these vars up to us,
	--	 so just choose a kind from the namespace now.
	let extraQuantVKs	
		= [(v, kindOfSpace $ Var.nameSpace v)
			| v	<- Set.toList free
			, not $ Set.member v quantVs]
		
	-- quantify free vars in the scheme		
	let tt_quant	= makeTForall_back extraQuantVKs tt'

	-- add read and write effects
	let ?newVarN	= newVarN
	tt_eff		<- elaborateEffT (Set.toList rsRead) (Set.toList rsWrite) tt_quant

	-- add closures
	tt_clo		<- elaborateCloT tt_eff
	
	-- make a new Mutable fetter for each region that is written to
	let fsMutable	= map (\r -> FConstraint Var.primMutable [TVar KRegion r])
			$ Set.toList rsWrite
			
	let tt_fs	= addFetters fsMutable tt_clo
	
	trace 	( "elaborateT\n"
		% "     tt = " % tt 			% "\n"
		% "   read = " % rsRead 		% "\n"
		% "  write = " % rsWrite	 	% "\n"
		% "   free = " % free			% "\n"
		% "    tt':\n" %> prettyTS tt_fs	% "\n")
		$ return tt_fs

-- Collect the regions which are read/written in this type and short out the
--	TElaorate's while we're here.
collectRsRW :: Type -> (Type, (Set Var, Set Var))
collectRsRW tt
	= runState (transformTM collectRsRW1 tt) (Set.empty, Set.empty)
	

collectRsRW1 tt
 = case tt of
	TElaborate ElabRead t
	 -> do	modify $ \(rd, wr) -> 
			( Set.union rd $ freeVarsR t
			, wr)
		return t
		
	TElaborate ElabWrite t
	 -> do	modify $ \(rd, wr) -> 
			( rd
			, Set.union wr $ freeVarsR t)
		return t
		
	TElaborate ElabModify t
	 -> do	modify $ \(rd, wr) -> 
			let free = freeVarsR t 
			in ( Set.union rd free
		   	   , Set.union wr free)
		return t
	
		
	_ -> return tt

freeVarsR tt
	= Set.filter (\r -> Var.nameSpace r == NameRegion) 
	$ freeVars tt

-- State -------------------------------------------------------------------------------------------
data ElabS
	= ElabS 
	{ stateVarGen	:: Var.VarBind }

stateInit unique
	= ElabS
	{ stateVarGen	= Var.XBind unique 0 }
	
type ElabM = State ElabS

-- | Create a fresh variable
newVarN :: NameSpace -> ElabM Var
newVarN space
 = do	varId@(Var.XBind p i)	<- gets stateVarGen
 
	let name	= Var.namePrefix space ++ p ++ show i
	let var		= (Var.new name) 
			{ Var.bind 	= varId
			, Var.nameSpace = space }
	
	modify $ \s -> s { stateVarGen = Var.XBind p (i + 1) }
	
	return var
