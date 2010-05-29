
-- | Introduce XLocal constructors to bind free region variables.
--	The XLocal's for a particular variables are placed just before the deepest XDo
--	block that contains all the references to that region.
--
module Core.Bind
	(bindGlob)
where
import Core.Plate.Trans
import Core.Plate.FreeVars
import Core.Exp
import Core.Glob
import Core.Util
import Core.Reconstruct
import Shared.VarPrim
import Shared.VarGen
import Util
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Var
import Prelude				hiding	(mapM)
import Type.Util.Bits			(takeVarOfBind)
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Debug.Trace		as Debug

stage	= "Core.Bind"

debug	= False
trace ss xx
 = if debug 
 	then Debug.trace (pprStrPlain ss) xx
	else xx


type   BindM	= VarGenM


-- | Introduce local region definitions.
bindGlob 
	:: ModuleId
	-> String			-- ^ unique prefix to use for fresh vars.
	-> Map Var [Var]		-- ^ a map of all the class constraints acting on a particular region.
	-> Set Var			-- ^ the regions with global lifetime
	-> Glob				-- ^ the module glob
	-> Glob			

bindGlob mod unique classMap rsGlobal tree
 = 	evalVarGen (bindM mod classMap rsGlobal tree) ("w" ++ unique)


-- | Bind local region variables in this tree.
bindM	:: ModuleId
	-> Map Var [Var]
	-> Set Var
	-> Glob
	-> BindM Glob
	
bindM mod classMap rsGlobal glob
 = do	let ?classMap	= classMap

	-- bind regions with local scope
	let comp :: Top -> BindM Top
	    comp p	= liftM fst $ bindP rsGlobal p

	glob_local <- mapBindsOfGlobM comp glob

	-- add bindings for global regions
	-- TODO: don't rebind regions already bound in the header.
	psRegionsGlobal	
		<- liftM Map.fromList
		$  mapM (\vR -> do
				let tR	= TVar kRegion $ UVar vR
				ws	<- makeWitnesses tR classMap
				return	$ (vR, PRegion vR ws))
		$ Set.toList rsGlobal
	
	return	$ glob_local
		{ globRegion	= Map.union (globRegion glob) psRegionsGlobal }


-- | Bind local region variables in this top level thing
bindP 	:: (?classMap :: Map Var [Var])
	=> Set Var 			-- unbound vars which are not local to this top
					-- and cannot be bound here
	-> Top 
	-> BindM 
		( Top
		, Set Var)		-- vars which are still free in this top

bindP	shared pp
 = case pp of
 	PBind v x	
	 | canBindX x
	 -> do	(x', vsFree, vsLocal)	<- bindX shared x
	 	return	(PBind v x', vsFree)
	_
	 -> return (pp, Set.empty)


-- | Decend into this expression, 
--	Tracking which unbound region variables are non-local to this expression.
--	We used this to add letregions to bind local regions to the inner most function
--	they are free in.
--
bindX 	:: (?classMap :: Map Var [Var])
	=> Set Var		-- unbound variables which are non-local to this expression
	-> Exp
	-> BindM 
		( Exp		-- the new expression
		, Set Var	-- regions free in this expression
		, Set Var)	-- regions that were bound locally in this expression

bindX 	shared xx
 = case xx of
	XLAM b k x	
	 -> do	-- check for regions bound by lambdas
		let Just v	= takeVarOfBind b

		-- mark regions in kinds as shared because they are not local
		--	to the body of the lambda expression 
	 	let sharedKs	= freeRegions k

		let shared'	= addSharedVs sharedKs
				$ addSharedV  v 
				$ shared

	 	(x', vsFree, vsLocal)	<- bindX shared' x
	 	return	( XLAM b k x'
			, Set.delete v vsFree
			, vsLocal )

	XTau t x
	 -> do	let sharedT	= freeVars t
	 	    shared'	= Set.union sharedT shared

	 	(x', vsFree, vsLocal)	<- bindX shared' x
	 	return	( XTau t x'
			, addSharedVs 
				(freeVars t)
				vsFree
			, vsLocal)

	XLam v t x eff clo
	 -> do	(x', vsFree, vsLocal)	<- bindX shared x

		-- mask out effects which are known to be local to the body of this expression
		let effMasked	= 
			packT
			$ transformT
				(\tt -> case tt of
					  TApp t1 (TVar kRegion (UVar r))
						|   elem t1 [tRead, tWrite]
						 && Set.member r vsLocal
						-> tPure
				 	  _	-> tt)
				eff

	 	return	( XLam v t x' effMasked clo
			, addSharedVs 
				(freeVars t)
				vsFree
			, vsLocal)

	-- TODO: push regions used only in a single alternative down 
	--	 into that alternative. Better if we can refactor the trash in 
	--	 bindXDo do be more general, then use that.
	XMatch aa
	 -> do	(aa', vssFree, vssLocal)	
	 		<- liftM unzip3 $ mapM (bindA shared) aa

	 	return	( XMatch aa'
			, Set.unions vssFree
			, Set.unions vssLocal)

	XDo ss	-> bindXDo shared xx

	XLocal r vts x
	 -> do	(x', vsFree, vsLocal)	<- bindX shared x
	  	return	( XLocal r vts x
	 		, Set.delete r vsFree
			, Set.insert r vsLocal)

	_	
	 -> 	return	( xx
	 		, freeRegionsX xx
	 		, Set.empty )


-- TODO: push regions used in only a single guard down into that guard
--	 also regions used in only the case object.
bindA shared aa@(AAlt gs x)
 = do	(gs', vssFreeGs, vssLocalGs)	<- liftM unzip3 $ mapM (bindG shared) gs
 	(x',  vsFreeX, vsLocalX)	<- bindX shared x

	let aa'	= AAlt gs' x'

	return	( aa'
		, Set.unions (vsFreeX  : vssFreeGs) 
		, Set.unions (vsLocalX : vssLocalGs))
	
bindG shared (GExp w x)
 = do	(w', vsFreeW, vsLocalW)	<- bindW shared w

	-- For the types on constant patterns, the match expression will have a read
	--	effects on regions in those types. These are not local to the RHS 
	--	expression.
	let sharedX		= Set.union shared vsFreeW
	(x', vsFree,  vsLocal)	<- bindX sharedX x
	
 	return	( GExp w' x'
		, Set.union vsFree vsFreeW
		, Set.union vsLocal vsLocalW)


bindW shared ww
 = case ww of
	WVar v
	 -> return	( WVar v
	 		, Set.empty
			, Set.empty )

 	WLit sp l
	 -> return 	( WLit sp l
	 		, Set.empty
			, Set.empty )
	WCon sp v lts
	 -> return	( WCon sp v lts
	 		, Set.unions $ map (freeRegions . t3_3) lts
			, Set.empty)


-- | Bind local regions in this XDo expression
bindXDo 
	:: (?classMap :: Map Var [Var])
	=> Set Var			-- the regions which are not-local to this expression
	-> Exp
	-> BindM
		( Exp			-- new expression
		, Set Var		-- the regions free in this expresion
		, Set Var)		-- regions bound locally in this expression

bindXDo shared xx@(XDo ss)
 = trace 
 	( pprStrPlain
 	$ "bindXDo_enter\n" 
	% "  stmts        = " % (catMaybes $ map takeStmtBoundV ss) 	% "\n"
	% "  shared above = " % (Set.toList shared)			% "\n"
	% "\n")
 $ do	
  	-- On the way down, calculate which regions are free in each statement.
	let ivsFree_down = 
  		zipWith (\i s -> (i, freeRegionsS s))
			([0..] :: [Int])
			ss
	
  	-- Decend into each statement, passing down the set of regions which are local to it.
	let decendS :: Int -> Stmt -> BindM (Stmt, Set Var, Set Var)
	    decendS ix (SBind mV x) = do
		-- work out the regions which are shared with other bindings in this group
		let sharedGroup	= Set.unions
				[ss	| (i, ss)	<- ivsFree_down
					, i /= ix]
	
		-- the new non-local vars
		let sharedHere	= Set.union shared sharedGroup
	
		(x', vsFreeX, vsLocalX)	<- bindX sharedHere x
		return	$ ( SBind mV x'
				, vsFreeX
				, vsLocalX)

	(ss', vssFree_stmts, vssLocal_stmts)
			<- liftM unzip3
			$  zipWithM
				decendS
				[0..]
				ss

	-- These are all the variables which are /still/ free in the statements, after decending into them
	--	We can't use reuse ivsFree_down because the vars which it contains might have been
	--	locally bound in a stmt and therefore no longer free in it.
	let vsFree_stmts	= Set.unions vssFree_stmts
	let ivsFree_up		= zip ([0..] :: [Int]) vssFree_stmts

	-- If a variable is local to this do block, but used in multiple statements then
	--	we can't push the binding down any further - or it will be out of scope in
	--	on of our statements. These are the vars that get bound right here.
	--
	let expand (i, vSet)	= [ (v, i)	| v <- Set.toList vSet ]
	let vsUsage		= gather $ catMap expand ivsFree_up

	-- these are the regions which are used in multiple statements
	let vsSplit		= [ v	| (v, uses)	<- vsUsage
					, (case uses of
						[]	-> False
						[_]	-> False
						_	-> True)]

	-- these are regions which are free in a certain statement, but that
	--	statement doesn't have an XDo form, so they can't be bound further down.

	--	We need this to detect the situation where a region is only used in a single 
	--	statement (so isn't in vsSplit), but still can't be bound in that statement.
	
	let vsBindForce		= Set.unions
				$ [ vsFree	| (SBind _ x, vsFree)	<- zip ss' vssFree_stmts
						, not (canBindX x) ]
	
	-- Regions visible in the type of the expression can't be bound here
	let vsVisible		= freeVars $ reconX_type "Core.Bind.bindXDo" xx
		
	-- the regions to bind on this XDo
	let vsBindHere	= 
		(Set.fromList vsSplit `Set.union` vsBindForce)
			 `Set.difference` (Set.union shared vsVisible)

	-- the new expression
	x'	<- bindRegionsX vsBindHere $ XDo ss'

	-- we've just bound some regions, so remove them from 
	--	the set of regions free in this expression
	let vsFree	= Set.difference vsFree_stmts vsBindHere

	trace	( pprStrPlain 
			$ "bindXDo_leave\n"
			%  "  stmts        = " % (catMaybes $ map takeStmtBoundV ss) 			% "\n"
			%  "  shared above = " % (Set.toList shared)					% "\n"
			%  "  vars free in each stmt (down)\n"
			%>	ivsFree_down			% "\n\n"

			%  "  vars free in each stmt (up)\n"
			%>	("\n" %!% (map (\(v, s) -> v % " " % Set.toList s) ivsFree_up))		% "\n\n"
			%  "  region usage for each statement\n"
			%>  vsUsage									% "\n\n"
			%  "  regions shared between statements\n"
			%>  vsSplit									% "\n\n"
			%  "  regions that can't be bound by their statements\n"
			%>  Set.toList vsBindForce							% "\n\n"
			%  "  regions to bind here\n"
			%>  Set.toList vsBindHere							% "\n\n"
			% "   regions still free\n"
			%>  Set.toList vsFree								% "\n\n"
			%  "  statements\n\n"
			%>  ("\n\n" %!% ss')								% "\n\n"
			% "\n")
		$ return ()
	
	return 	( x'
		, vsFree
		, Set.unions (vsBindHere : vssLocal_stmts))
 


bindRegionsX
	:: (?classMap :: Map Var [Var])
	=> Set Var					-- ^ regions to bind
	-> Exp 						-- ^ expression to wrap in letregion
	-> BindM Exp	
	
bindRegionsX rsLocal xx
 = do	
	-- Check for fetters on these regions.
	rsLocalFs	
		<- mapM 
			(\r 
			 -> do  let r'	= TVar kRegion $ UVar r
				fs'	<- makeWitnesses r' ?classMap
				return 	(r', fs'))
		$ Set.toList rsLocal

	-- Append local region definitions to the expression.
	--	Reverse the order of there regions here so they come out 
	--	in the same order as they're present in the block.
	--	The actual order doesn't matter.
	--
	let xx'	= foldl
			(\x (TVar _ (UVar v), fs) -> XLocal v fs x)
			xx
			(reverse rsLocalFs)
			
   	return	xx'


-- | Construct appropriate witnesses for the constraints on this region variable.
makeWitnesses
	:: Region		-- ^ the region variable
	-> Map Var [Var]	-- ^ map of what region classes are supported
	-> BindM 
		[(Var, Type)]	-- ^ map of witness variable to a type expresison that
				--	constructs the witness.

makeWitnesses r@(TVar k (UVar vR)) classMap
 | k == kRegion
 = do
	-- lookup the constraints on this region
	let Just vsConstraints	= Map.lookup vR classMap
	

	-- Mutable vs Const -----------------------------------------------------
	-- default regions to const.
	let gotMutable		= elem primMutable vsConstraints
	let gotConst		= elem primConst   vsConstraints

	-- sanity check
	when (gotMutable && gotConst)
	 $ panic stage $ "makeWitnesses: Region " % r % " is both Mutable and Const\n"

	vWitnessMC		<- newVarN NameClass

	let witnessMC		=
	  	if gotMutable	then (vWitnessMC, TApp tMkMutable r)
				else (vWitnessMC, TApp tMkConst   r)
				

	-- Lazy vs Direct -------------------------------------------------------
	-- default regions to direct.
	let gotLazy		= elem primLazy   vsConstraints
	let gotDirect		= elem primDirect vsConstraints
	
	-- sanity check
	when (gotLazy && gotDirect)
	 $ panic stage $ "makeWitnesses: Region " % r % " is both Lazy and Direct\n"
	 
	vWitnessLD		<- newVarN NameClass
	
	let witnessLD		=
		if gotLazy	then (vWitnessLD, TApp tMkLazy   r)
				else (vWitnessLD, TApp tMkDirect r)
	
  	return	[witnessMC, witnessLD]


-- 
canBindX :: Exp -> Bool
canBindX xx
 = case xx of
 	XLAM v t x		-> canBindX x
	XAPP{}			-> False
	XTau t x		-> canBindX x
	XLam v t x eff clo	-> canBindX x
	XApp{}			-> False
	XDo{}			-> True
	XMatch{}		-> False
	XLit{}			-> False
	XVar{}			-> False
	XLocal v ls x		-> canBindX x
	XPrim{}			-> False
	XProject{}		-> False	


addSharedV v vsShared
	| varNameSpace v == NameRegion
	= Set.insert v vsShared
	
	| otherwise
	= vsShared

addSharedVs vs vsShared	
 = 	Set.union
 		(Set.filter (\v -> varNameSpace v == NameRegion) vs)
		vsShared

takeStmtBoundV ss
 = case ss of
 	SBind mv ss -> mv

freeRegions zz
	= Set.filter vIsRegion
	$ freeVars zz

freeRegionsX xx	
	= Set.filter vIsRegion 
	$ freeVars xx

freeRegionsS xx	
	= Set.filter vIsRegion 
	$ freeVars xx


vIsRegion v = varNameSpace v == NameRegion
