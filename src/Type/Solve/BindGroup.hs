
module Type.Solve.BindGroup
	(bindGroup)
where
import Type.State
import Constraint.Exp
import Util
import Util.Graph.Deps
import DDC.Var
import qualified Data.Set	as Set
import qualified Data.Map	as Map

-----
debug	= True
trace s	= when debug $ traceM s

-- Use the current contains and instantiates maps in the solver state to work out what
--	bindings are in this one's bind group. ie, what set of mutually recursive bindings this
--	one belongs to.
--
--	This should only be called when generalising this binding, because it uses the current
--	path to work out what other variables were let bound at the same scope.
--
--	returns 	Nothing		if the binding is not recursive
--	otherwise	Just vs		where vs are the vars of the let bindings which are
--					recursive with this one
--
bindGroup 
	:: Var
	-> SquidM (Maybe (Set Var))
	
bindGroup vBind
 = {-# SCC "bindGroup" #-} bindGroup' vBind

bindGroup' vBind
 = do
	trace	$ "*   bindGroup " % vBind % "\n"
	let bBind	= BLet [vBind]

	-- Grab what path we're on and follow it back up until we find the CBind that tells
	--	us what other variables were let-bound at the same level as this one.
	path		<- getsRef statePath
--	trace	$ "    path          = " % path	% "\n"

	let mbUseGroup	= find	(\b -> case b of
					BLetGroup vs		-> elem vBind vs
					_			-> False)
					path

	-- if there is no let group on the path this means the binding was defined 
	--	but never instantiated..
	let bUseGroup@(BLetGroup vsLetGroup)
			= case mbUseGroup of
				Nothing	-> BLetGroup []
				Just bb	-> bb

--	trace	$ "    bUseGroup     = " % bUseGroup			% "\n\n"

	-- Build the constraint dependency graph.
	--	This is a union of the contains and instantiates map.

	-- Lookup the branch containment graph and prune it down to just those branches
	--	reachable from the let group containing the binding to generalise.
	gContains_	<- getsRef stateContains
	let gContains	= graphPrune gContains_ bUseGroup
	let sContains	= Set.unions (Map.keysSet gContains : Map.elems gContains)

	-- Lookup the branch instantiation graph and prune it down to just those members
	--	that are present in the containment graph. 
	--	Also restrict it to just instantiations of let bindings. 
	gInst1		<- getsRef stateInstantiates
	let gInst2	= Map.filterWithKey (\k a -> Set.member k sContains) gInst1
	let gInst	= Map.map (Set.filter (\b -> b =@= BLet{})) gInst2

	-- The dependency graph is the union of both of these.
	let gDeps	=  Map.unionWith (Set.union) gContains gInst

--	trace	$ "    gContains:\n" 		%> ppr gContains	% "\n\n"
--		% "    gInst:\n"		%> ppr gInst		% "\n\n"		
--		% "    gDeps:\n"		%> ppr gDeps		% "\n\n"
	-- Work out all the stronly connected components (mutually recursive groups) 
	--	reachable from the binding which needs its type generalised.
	let sccs_all	= graphSCC gDeps bBind
--	trace	$ "    sccs_all    = " % sccs_all	% "\n"
	
	-- Only keep the sccs that this binding is actually a member of.
	--	We need to this otherwise we'll also pick up mutually recursive groups defined 
	--	at the same level, but which vBind isn't actually a member of.
	let sccs_member	= filter (Set.member bBind) sccs_all
-- 	trace	$ "    sccs_member = " % sccs_member	% "\n"

	-- Pack all the individual sccs together, they're in the same binding group.
	let scc		= Set.unions sccs_member

	-- The containment graph contains information about lambda bindings as well.
	--	They're in the scc, but we only care about let bindings here.
	let scc_let
		= Set.fromList
		$ catMaybes
		$ map	(\b -> case b of
				BLet [v] 
				 | elem v vsLetGroup	-> Just v
				_	 		-> Nothing)
		$ Set.toList scc

--	trace	$ "    scc_let     = " % scc_let	% "\n\n"

	if Set.null scc_let 
		then return Nothing
		else return $ Just scc_let
