
module Desugar.Elaborate 
	(elaborateTree)
where
import Desugar.Exp
import Desugar.Pretty
import Type.Util.Elaborate
import Type.Util.Bits
import Type.Util.Kind
import Type.Plate.FreeVars
import Type.Plate.Trans
import Type.Pretty
import Type.Exp
import Control.Monad.State.Strict
import Util
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Var
import qualified Debug.Trace
import qualified Data.Set	as Set
import qualified Shared.VarUtil	as Var
import qualified Shared.VarPrim	as Var

-----
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
  = trace ("elaborateP: " % stripAnnot pp)
  $ case pp of
	PExtern sp v t mt
	 -> do	t'	<- elaborateT t
		return	$ PExtern sp v t' mt
		
	PTypeSig a vs t
	 -> do	t'	<- elaborateT t
		return	$ PTypeSig a vs t'
	
	_ -> return pp

elaborateT :: Type -> ElabM Type
elaborateT tt
 = trace ("elaborateT: " % tt)
 $ case tt of
	TApp{}
	 | Just _	<- takeTFun tt
	 -> elaborateT_fun tt

	TFetters t1 fs 	
	 | Just _	<- takeTFun t1
	 -> elaborateT_fun tt

	_	-> return tt

elaborateT_fun :: Type -> ElabM Type
elaborateT_fun tt
 = trace ("elaborateT_fun: " % tt)
 $ do	let (tt', (rsRead, rsWrite))	
			= collectRsRW tt

	let free	= Set.filter (not . Var.isCtorName) $ freeVars tt'
	
	-- see what vars are already quantified in this scheme
	let (bks, _)	= slurpTForall tt'
	let quantVs	= Set.fromList $ map (varOfBind . fst) bks
	
	(tt_rs, newRs)	<- elaborateRsT newVarN tt'
	
	trace ("tt_rs = " % tt_rs) $ do
	
	-- TODO: freeVars doesn't pass the kinds of these vars up to us,
	--	 so just choose a kind from the namespace now.
	let extraQuantVKs	
		= [(v, kindOfSpace $ varNameSpace v)
			| v	<- Set.toList free
			, not $ Set.member v quantVs]
		++ newRs
		
	-- quantify free vars in the scheme		
	let tt_quant	= makeTForall_back extraQuantVKs tt_rs

	-- add read and write effects
	let ?newVarN	= newVarN
	tt_eff		<- elaborateEffT (Set.toList rsRead) (Set.toList rsWrite) tt_quant

	-- add closures
	tt_clo		<- elaborateCloT tt_eff
	
	-- make a new Mutable fetter for each region that is written to
	let fsMutable	= map (\r -> FConstraint Var.primMutable [TVar kRegion r])
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
	= Set.filter (\r -> varNameSpace r == NameRegion) 
	$ freeVars tt

-- State -------------------------------------------------------------------------------------------
data ElabS
	= ElabS 
	{ stateVarGen	:: VarId }

stateInit unique
	= ElabS
	{ stateVarGen	= VarId unique 0 }
	
type ElabM = State ElabS

-- | Create a fresh variable
newVarN :: NameSpace -> ElabM Var
newVarN space
 = do	vid@(VarId p i)	<- gets stateVarGen
 
	let name	= charPrefixOfSpace space : p ++ show i
	let var		= (varWithName name) 
			{ varId 	= vid
			, varNameSpace 	= space }
	
	modify $ \s -> s { stateVarGen = VarId p (i + 1) }
	
	return var
