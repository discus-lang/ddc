
module Core.Bind
	( bindTree )

where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import qualified Shared.VarBind	as Var
import Shared.VarPrim
import Shared.VarGen
import Shared.Error

import Util
import Core.Plate.Trans
import Core.Plate.Walk
import Core.Plate.FreeVars
import Core.Exp
import Core.Util
import Core.Pretty


import qualified Debug.Trace	as Debug

type   BindM	= VarGenM

-----
stage	= "Core.Bind"
debug	= False
trace ss xx
 = if debug 
 	then Debug.trace (pretty ss) xx
	else xx


-- | Introduce local region definitions.
bindTree 
	:: String			-- ^ unique prefix to use for fresh vars
	-> (Var -> Maybe [Class])
	-> Tree				-- ^ core tree
	-> Tree			

bindTree unique lookupFs tree
 = let	?lookupFs	= lookupFs
   in	evalVarGen 
		(mapM (\p -> liftM fst $ bindP Set.empty p) tree)
		("w" ++ unique)

-----
bindP 	:: (?lookupFs :: Var -> Maybe [Class]) 
	=> Set Var 		-- unbound vars which are not local to this top
				-- and cannot be bound here
	-> Top 
	-> BindM 
		( Top
		, Set Var)	-- vars which are still free in this top

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
bindX 	:: (?lookupFs :: Var -> Maybe [Class])
	-> Set Var		-- unbound variables which are non-local to this expression
	-> Exp
	-> BindM 
		( Exp		-- the new expression
		, Set Var	-- regions free in this expression
		, Set Var)	-- regions that were bound locally in this expression

bindX 	shared xx
 = case xx of
	XLAM b t x	
	 -> do	-- check for regions bound by lambdas
		let v		= varOfBind b
	 	let shared'	= addSharedV v shared

	 	(x', vsFree, vsLocal)	<- bindX shared' x
	 	return	( XLAM b t x'
			, Set.delete v vsFree
			, vsLocal )

	-- When we hit an XTet on the way back up, mask out
	--	any effects which are known to be local to some sub-expression.
	XTet vts x	
	 -> do	(x', vsFree, vsLocal)	<- bindX shared x
		let vts'	= 
			transformT
				(\tt -> case tt of
					  TEffect v [TVar KRegion r] 
						|   elem v [primRead, primWrite]
						 && Set.member r vsLocal
						-> TBot KEffect

					  _	-> tt)
				vts

		trace	( "masking XTet\n"
			% "  vts     = " % vts 			% "\n"
			% "  vts'    = " % vts'			% "\n"
			% "  vsLocal = " % Set.toList vsLocal	% "\n")
		 $ return
			( XTet vts' x'
			, addSharedVs 
				(Set.unions $ map (freeVarsT . snd) vts)
				vsFree
			, vsLocal)

	XTau t x
	 -> do	(x', vsFree, vsLocal)	<- bindX shared x
	 	return	( XTau t x'
			, addSharedVs 
				(freeVarsT t)
				vsFree
			, vsLocal)

	XLam v t x eff clo
	 -> do	(x', vsFree, vsLocal)	<- bindX shared x
	 	return	( XLam v t x' eff clo
			, addSharedVs 
				(freeVarsT t)
				vsFree
			, vsLocal)

	-- BUGS: handle regions bound in different alternatives
	XMatch aa eff
	 -> do	(aa', vssFree, vssLocal)	
	 		<- liftM unzip3 $ mapM (bindA shared) aa

	 	return	( XMatch aa' eff
			, Set.unions vssFree
			, Set.unions vssLocal)

	XDo ss	-> bindXDo shared xx

	_	
	 -> 	return	( xx
	 		, freeRegionsX xx
	 		, Set.empty )


-- BUGS: shared regions in local funs in case expr
bindA shared (AAlt gs x)
 = do	(gs', vssFreeGs, vssLocalGs)	<- liftM unzip3 $ mapM (bindG shared) gs
 	(x',  vsFreeX, vsLocalX)	<- bindX shared x
	
	return	( AAlt gs' x'
		, Set.unions (vsFreeX  : vssFreeGs) 
		, Set.unions (vsLocalX : vssLocalGs))
	
bindG shared (GExp w x)
 = do	(x', vsFree, vsLocal)	<- bindX shared x
 	return	( GExp w x'
		, vsFree
		, vsLocal)



-- | Bind local regions in this XDo expression
bindXDo 
	:: (?lookupFs :: Var -> Maybe [Class])
	=> Set Var			-- the regions which are not-local to this expression
	-> Exp
	-> BindM
		( Exp			-- new expression
		, Set Var		-- the regions free in this expresion
		, Set Var)		-- regions bound locally in this expression

bindXDo shared xx@(XDo ss)
 = trace 
 	( pretty 
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
	
  	-- Decend into each statement, passing down the set of regions which a local to it.
	let decendS :: Int -> Stmt -> BindM (Stmt, Set Var, Set Var)
	    decendS ix (SBind mV x) = do
		-- work out the regions which are shared with other bindings in this group
		let sharedGroup	= Set.unions
				[ss	| (i, ss)	<- ivsFree_down
					, i /= ix]
	
		-- the new non-local vars
		let sharedHere	= Set.union shared sharedGroup
	
		(x', vsFree, vsLocal)	<- bindX sharedHere x
		return		( SBind mV x'
				, case mV of
					Nothing	-> vsFree
					Just v 	-> Set.delete v vsFree
				, vsLocal)

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
	
	-- the regions to bind on this XDo
	let vsBindHere	= 
		(Set.fromList vsSplit `Set.union` vsBindForce)
			 `Set.difference` shared

	-- the new expression
	x'	<- bindRegionsX vsBindHere $ XDo ss'

	-- we've just bound some regions, so remove them from 
	--	the set of regions free in this expression
	let vsFree	= Set.difference vsFree_stmts vsBindHere

	trace	( pretty 
			$ "bindXDo_leave\n"
			%  "  stmts        = " % (catMaybes $ map takeStmtBoundV ss) 			% "\n"
			%  "  shared above = " % (Set.toList shared)					% "\n"
			%  "  vars free in each stmt (down)\n"
			%>	("\n" %!% (map (\(v, s) -> v % " " % Set.toList s) ivsFree_down))	% "\n\n"
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

			% "\n")
	 $ return
	 	( x'
		, vsFree
		, Set.unions (vsBindHere : vssLocal_stmts))
 


bindRegionsX
	:: (?lookupFs :: Var -> Maybe [Class]) 
	-> Set Var					-- ^ regions to bind
	-> Exp 						-- ^ expression to wrap in letregion
	-> BindM Exp	
	
bindRegionsX rsLocal xx
 = do	
	-- Check for fetters on these regions.
	rsLocalFs	
		<- mapM 
			(\r 
			 -> do  let fs	= getFetters r ?lookupFs
				let r'	= TVar KRegion r
				fs'	<- makeWitnesses r' fs
				return 	(r', fs'))
		$ Set.toList rsLocal

	-- Append local region definitions to the expression.
	--	Reverse the order of there regions here so they come out 
	--	in the same order as they're present in the block.
	--	The actual order doesn't matter.
	--
	let xx'	= foldl
			(\x (TVar KRegion v, fs) -> XLocal v fs x)
			xx
			(reverse rsLocalFs)
			
   	return	xx'

getFetters r lookupFetters 
 = []



-- Construct appropriate witnesses for this regions constraints
makeWitnesses
	:: Region
	-> [Class]
	-> BindM [(Var, Class)]

makeWitnesses r fs
 = do
	-- default regions to const.
	let gotMutable	= or $ map (\(TClass v _) -> Var.bind v == Var.FMutable) fs
	let gotConst	= or $ map (\(TClass v _) -> Var.bind v == Var.FConst)	 fs

	defaultFsC <-
	   if not gotMutable && not gotConst
		then do	v	<- newVarN NameClass
			return	[(v, TClass primConst [r])]
		else return []

	-- default regions to direct
	let gotLazy	= or $ map (\(TClass v _) -> Var.bind v == Var.FLazy)	fs
	let gotDirect	= or $ map (\(TClass v _) -> Var.bind v == Var.FDirect)	fs
	
	defaultFsD <-
	   if not gotLazy && not gotDirect
		then do	v	<- newVarN NameClass
			return	[(v, TClass primDirect [r])]
		
		else return []

  	return $ defaultFsC ++ defaultFsD


-- 
canBindX :: Exp -> Bool
canBindX xx
 = case xx of
 	XLAM v t x		-> canBindX x
	XAPP{}			-> False
	XTet vts x		-> canBindX x
	XTau t x		-> canBindX x
	XLam v t x eff clo	-> canBindX x
	XApp{}			-> False
	XDo{}			-> True
	XMatch{}		-> False
	XConst{}		-> False
	XVar{}			-> False
	XLocal v ls x		-> canBindX x
	XPrim{}			-> False
	


addSharedV v vsShared
	| Var.nameSpace v == NameRegion
	= Set.insert v vsShared
	
	| otherwise
	= vsShared

addSharedVs vs vsShared	
 = 	Set.union
 		(Set.filter (\v -> Var.nameSpace v == NameRegion) vs)
		vsShared

takeStmtBoundV ss
 = case ss of
 	SBind mv ss -> mv


freeRegionsX xx	
	= Set.filter vIsRegion 
	$ freeVarsX xx

freeRegionsS xx	
	= Set.filter vIsRegion 
	$ freeVarsS xx

vIsRegion v = Var.nameSpace v == NameRegion
