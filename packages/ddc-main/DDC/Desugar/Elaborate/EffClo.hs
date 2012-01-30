{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Desugar.Elaborate.EffClo
	(elaborateEffCloInFunSigT)
where
import DDC.Desugar.Elaborate.State
import DDC.Main.Pretty
import DDC.Type
import DDC.Type.Transform
import DDC.Var
import DDC.Util.FreeVars
import qualified Debug.Trace
import qualified Data.Set	as Set
import qualified Shared.VarUtil	as Var
import qualified Shared.VarPrim	as Var
import Util

debug		= False
trace ss xx	= if debug then Debug.Trace.trace (pprStrPlain ss) xx else xx

-- | Fill in effect and closure information in a type signature.
--   NOTE: As we're only filling in missing effect and closure information we
--         only have to worry about function types.
elaborateEffCloInFunSigT :: Type -> ElabM Type
elaborateEffCloInFunSigT tt
 = case tt of
	TApp{}
	 | Just _	<- takeTFun tt
	 -> elaborateEffCloInSigT tt

	TConstrain t1 _
	 | Just _	<- takeTFun t1
	 -> elaborateEffCloInSigT tt

	_	-> return tt


-- | Elaborate some type signature.
elaborateEffCloInSigT :: Type -> ElabM Type
elaborateEffCloInSigT tt
 = do	let (tt', (rsRead, rsWrite))
			= collectRsRW tt

	let free	= Set.filter (not . Var.isCtorName) $ freeVars tt'

	-- see what vars are already quantified in this scheme
{-	let (bks, _)		= takeTForall tt'
	let Just quantVs	= liftM Set.fromList
				$ sequence
				$ map (takeVarOfBind . fst) bks
-}
	(tt_rs, _)		<- elaborateRsT newVarN tt'

{-	-- TODO: freeVars doesn't pass the kinds of these vars up to us,
	--	 so just choose a kind from the namespace now.
	let extraQuantBKs
		= [(BVar v, k)
			| v	<- Set.toList free
			, not $ Set.member v quantVs
			, let Just k	= kindOfSpace $ varNameSpace v]
		++ [(BVar v, k) | (v, k) <- newRs]

	-- quantify free vars in the scheme
	let tt_quant	= makeTForall_back extraQuantBKs tt_rs
-}
	-- add read and write effects
	tt_eff		<- elaborateEffT
				newVarN
				(Set.toList rsRead) (Set.toList rsWrite)
				tt_rs

	-- add closures
	tt_clo		<- elaborateCloT newVarN tt_eff

	-- add a Mutable constraint for each region that is written to.
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


-- | Collect regions of the parameters that are marked as being read from or written to.
--   This is from the {read} and {write} elaboration annotations.
--   Also erase the annotations while we're here.
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

 where freeVarsR t
	= Set.filter (\r -> varNameSpace r == NameRegion)
	$ freeVars t
