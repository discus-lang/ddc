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
--   TODO: This is fairly ad-hoc at the moment, we'll need more experience with it to
--         determine if these heuristics are what we actually want. In all cases the 
--         program should work if you add in all the required type information manually.
--
--   TODO: I expect we'll want to combine kind inference with this process in the long run.
-- 
module DDC.Desugar.Elaborate 
	(elaborateTree)
where
import DDC.Desugar.Elaborate.State
import DDC.Desugar.Exp
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Type
import DDC.Type.Transform
import DDC.Var
import DDC.Util.FreeVars
import qualified Debug.Trace
import qualified Data.Set	as Set
import qualified Shared.VarUtil	as Var
import qualified Shared.VarPrim	as Var
import Control.Monad.State.Strict
import Util


debug		= False
trace ss xx	= if debug then Debug.Trace.trace (pprStrPlain ss) xx else xx

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
		
	PTypeSig a vs t
	 -> do	t'	<- elaborateT t
		return	$ PTypeSig a vs t'
	
	_ -> return pp

elaborateT :: Type -> ElabM Type
elaborateT tt
 = case tt of
	TApp{}
	 | Just _	<- takeTFun tt
	 -> elaborateT_fun tt

	TConstrain t1 _
	 | Just _	<- takeTFun t1
	 -> elaborateT_fun tt

	_	-> return tt


elaborateT_fun :: Type -> ElabM Type
elaborateT_fun tt
 = do	let (tt', (rsRead, rsWrite))	
			= collectRsRW tt

	let free	= Set.filter (not . Var.isCtorName) $ freeVars tt'
	
	-- see what vars are already quantified in this scheme
	let (bks, _)		= takeTForall tt'
	let Just quantVs	= liftM Set.fromList 
				$ sequence
				$ map (takeVarOfBind . fst) bks
	
	(tt_rs, newRs)		<- elaborateRsT newVarN tt'
			
	-- TODO: freeVars doesn't pass the kinds of these vars up to us,
	--	 so just choose a kind from the namespace now.
	let extraQuantBKs	
		= [(BVar v, k)
			| v	<- Set.toList free
			, not $ Set.member v quantVs
			, let Just k	= kindOfSpace $ varNameSpace v]
		++ [(BVar v, k) | (v, k) <- newRs]
		
	-- quantify free vars in the scheme		
	let tt_quant	= makeTForall_back extraQuantBKs tt_rs

	-- add read and write effects
	tt_eff		<- elaborateEffT 
				newVarN
				(Set.toList rsRead) (Set.toList rsWrite) 
				tt_quant

	-- add closures
	tt_clo		<- elaborateCloT newVarN tt_eff
			
	
	-- make a new Mutable fetter for each region that is written to
	let fsMutable	= map (\r -> FConstraint Var.primMutable [TVar kRegion $ UVar r])
			$ Set.toList rsWrite
			
	let tt_fs	= pushConstraintsOther fsMutable tt_clo
	
	trace (vcat
	 	[ ppr "elaborateT"
		, "     tt = " % tt
		, "   read = " % rsRead
		, "  write = " % rsWrite
		, "   free = " % free
		, " tt_eff = " % tt_eff
		, " tt_clo = " % tt_clo
		, "    tt':\n" %> prettyTypeSplit tt_fs
		, blank])
		$ return tt_fs

-- Collect the regions which are read/written in this type and short out the
--	TElaorate's while we're here.
collectRsRW :: Type -> (Type, (Set Var, Set Var))
collectRsRW tt
	= runState (transformTM collectRsRW1 tt) (Set.empty, Set.empty)
	

collectRsRW1 tt
 = case tt of
	TApp (TCon (TyConElaborate TyConElaborateRead _)) t2
	 -> do	modify $ \(rd, wr) -> 
			( Set.union rd $ freeVarsR t2
			, wr)
		return t2
		
	TApp (TCon (TyConElaborate TyConElaborateWrite _)) t2
	 -> do	modify $ \(rd, wr) -> 
			( rd
			, Set.union wr $ freeVarsR t2)
		return t2
		
	TApp (TCon (TyConElaborate TyConElaborateModify _)) t2
	 -> do	modify $ \(rd, wr) -> 
			let free = freeVarsR t2 
			in ( Set.union rd free
		   	   , Set.union wr free)
		return t2
		
	_ -> return tt

freeVarsR tt
	= Set.filter (\r -> varNameSpace r == NameRegion) 
	$ freeVars tt
