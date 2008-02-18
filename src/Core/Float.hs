
module Core.Float
	( Table(..)
	, tableZero
	, Stats(..)
	, floatBindsTree
	, floatBindsTreeUse)

where

import qualified Data.Map		as Map
import Data.Map				(Map)

import qualified Data.Set		as Set
import Data.Set				(Set)

import Util
import Shared.VarPrim	
import Shared.VarGen
import Shared.Error
import Core.Exp
import Core.ReconKind
import Core.BoundUse
import qualified Core.Reconstruct	as Recon
import Core.Util
import Core.Util.Rename
import Core.Util.Slurp

import qualified Core.Plate.Trans	as Trans
import qualified Core.Plate.Walk	as Walk

import qualified Debug.Trace		as Debug

-----
stage	= "Core.Float"
debug	= False
trace ss x
 = if debug 
 	then Debug.trace (pprStr (stage % ":" % ss)) x
	else x


-- Table -------------------------------------------------------------------------------------------

-- How many value lambdas we are in from the top level.
type Level	= Int

-- | inliner table
data Table
	= Table
	{ -- vars of bindings to not float
	  tableNoFloat	 	:: Set Var

	  -- how each bound varable is uses
	  --	(obtained via Core.BoundUse)
	, tableBoundUse		:: Map Var [Use]

	  -- vars of regions that are known to be constant
	, tableConstRegions	:: Set Var

	  -- vars of types that are known to be (deep) constant
	, tableConstTypes	:: Set Var

	  -- vars of effects that are known to be pure
	, tablePureEffects	:: Set Var

 	  -- bindings currently in motion along with the effect of that binding.
	, tableBinds		:: Map Var (Exp, Effect)


	  -- stats about what got moved / not moved
	, tableStats		:: Stats }
	


-- empty inliner table
tableZero
	= Table
	{ tableNoFloat		= Set.empty
	, tableBoundUse		= Map.empty
	, tableConstRegions	= Set.empty
	, tableConstTypes	= Set.empty
	, tablePureEffects	= Set.empty
	, tableBinds		= Map.empty 
	
	, tableStats		= statsZero }

-- horror
tableStats_  = ( tableStats, \x s -> s { tableStats = x })


-- Share -------------------------------------------------------------------------------------------

-- This is some very simple CSE ---------
-- We could do this with a more general mechanism, but these are important enough, and fast enough
--	to embed directly in the let-floater.

-- We don't merge this with the Table above, because that contains reconstructed
--	bottom-up information as well. This sharing is only top-down.

-- shared results
data Share
	= Share
	{ -- variable substition
	  shareVarSub		:: Map Var Var

	  -- Elimination of forcing is important, because most function args are in
	  --	unknown regions and must be forced before use, and tend to have
	  --	multiple bound occurances inside the function.
	, shareForcings		:: Map Var Var		-- v2 = force v1

	  -- Elimination of unboxings follows from elimination of forcings
	  -- 	v2 = unbox v1
	  --	v3 is the region that the unboxed data was in
	  --	We can move unboxings from mutable regions, so long as we don't carry the result over
	  --	a statement that writes to that region.
	, shareUnboxings	:: Map Var (Var, Var)
	}

-- empty share table
shareZero
	= Share
	{ shareVarSub		= Map.empty
	, shareForcings		= Map.empty 
	, shareUnboxings	= Map.empty }

-- sink a variable using the share table
sinkVar :: Share -> Var -> Var
sinkVar share v1
 = case Map.lookup v1 (shareVarSub share) of
 	Just v2	-> sinkVar share v2
	_	-> v1


-- Stats -------------------------------------------------------------------------------------------
-- | status about how many bindings were moved / notmoved
--	these are lists instead of sets, to reduce the overhead of collecting up the stats.
data Stats
	= Stats
	{ -- total number of bindings inspected
	  statsTotalBindings	:: Int
	
	  -- bindings that were not moved because they were in the no-float table
	, statsNotMovedNoFloat	:: [Var]

	  -- bindings that were not moved because they had multiple uses
	, statsNotMovedMultiUse	:: [Var]

	  -- bindings that were not moved because their use was at a deeper lambda level
	, statsNotMovedDeepLambda :: [Var]
	
	  -- bindings that were not moved because they had top-level effects.
	, statsNotMovedTopLevel	:: [Var]
	
	  -- bindings that were not moved because they had no uses.
	, statsNotMovedNoUses	:: [Var]
	
	  -- bindings that were not moved, and all their uses were directly under unboxings
	  -- .. not all of these are total failures, because the stmt might not have boxed the value itself.
	, statsMissedUnboxing	:: [Var]
	
	  -- bindings that were successfully moved
	, statsMoved		:: [Var] 

	  -- calls to force successfully shared
	, statsSharedForcings	:: [Var]

	  -- unboxings successfully shared
	, statsSharedUnboxings	:: [Var] }

	
-- empty stats table
statsZero
	= Stats
	{ statsTotalBindings		= 0
	, statsNotMovedNoFloat		= []
	, statsNotMovedMultiUse		= []
	, statsNotMovedDeepLambda	= []
	, statsNotMovedTopLevel		= []
	, statsNotMovedNoUses		= []
	, statsMissedUnboxing		= []
	, statsMoved			= [] 
	
	, statsSharedForcings		= []
	, statsSharedUnboxings		= [] }

-- horror
statsTotalBindings_		= (statsTotalBindings, 		\x s -> s { statsTotalBindings 		= x})
statsNotMovedNoFloat_		= (statsNotMovedNoFloat, 	\x s -> s { statsNotMovedNoFloat	= x})
statsNotMovedMultiUse_		= (statsNotMovedMultiUse, 	\x s -> s { statsNotMovedMultiUse 	= x})
statsNotMovedDeepLambda_	= (statsNotMovedDeepLambda, 	\x s -> s { statsNotMovedDeepLambda 	= x})
statsNotMovedTopLevel_		= (statsNotMovedTopLevel, 	\x s -> s { statsNotMovedTopLevel 	= x})
statsNotMovedNoUses_		= (statsNotMovedNoUses, 	\x s -> s { statsNotMovedNoUses 	= x})
statsMissedUnboxing_		= (statsMissedUnboxing, 	\x s -> s { statsMissedUnboxing 	= x})
statsMoved_			= (statsMoved, 			\x s -> s { statsMoved		 	= x})
statsSharedForcings_		= (statsSharedForcings,		\x s -> s { statsSharedForcings	 	= x})
statsSharedUnboxings_		= (statsSharedUnboxings, 	\x s -> s { statsSharedUnboxings	= x})

	
instance Pretty Stats where
 ppr ss
 	= "Float.Stats:\n"
	% "  * total bindings inspected                : " % statsTotalBindings ss			% "\n"
	% "  * number of bindings moved                : " % (length $ statsMoved ss)			% "\n" 
	% "  * number of bindings not moved because they..\n"
	% "      . were in the no-float set            : " % (length $ statsNotMovedNoFloat ss)		% "\n"
	% "      . had multiple uses                   : " % (length $ statsNotMovedMultiUse ss)	% "\n"
	% "      . were used at a deeper lambda level  : " % (length $ statsNotMovedDeepLambda ss)	% "\n"
	% "      . had top level effects               : " % (length $ statsNotMovedTopLevel ss)	% "\n"
	% "      . had no uses                         : " % (length $ statsNotMovedNoUses ss)		% "\n"
	% "  * number of bindings not moved, \n"
	% "       and all their uses were unboxings    : " % (length $ statsMissedUnboxing ss)		% "\n"
	% "  * number of shared forcings               : " % (length $ statsSharedForcings ss)		% "\n"
	% "  * number of shared unboxings              : " % (length $ statsSharedUnboxings ss)		% "\n"
	

-- floatBinds -------------------------------------------------------------------------------------

-- | Calculate usage information and float the binds in this tree
floatBindsTreeUse
	:: Tree -> (Table, Tree)

floatBindsTreeUse tree
 = let	-- count the number of bound occurances of each variable
 	boundUse	= execState (boundUseTree tree) Map.empty

	-- float bindings into their use sites
	table		= tableZero { tableBoundUse = boundUse }

   in	floatBindsTree table tree
   
	


-- | Float the binds in this tree
floatBindsTree 
	:: Table -> Tree -> (Table, Tree)

floatBindsTree tt tree
	= mapAccumL (floatBindsP 0 shareZero) tt tree
   

-- Top ---------------------------------------------------------------------------------------------
floatBindsP :: Level -> Share -> Table -> Top -> (Table, Top)
floatBindsP level share tt pp
 = case pp of
 	PBind v x	
	 -> let	(tt', x')	= floatBindsX level share tt x
	    in	(tt', PBind v x')

	_		-> (tt, pp)
	
-- Exp ---------------------------------------------------------------------------------------------
floatBindsX :: Level -> Share -> Table -> Exp -> (Table, Exp)
floatBindsX level share tt xx
 = case xx of

	-- check for constant regions on the way down
 	XLAM b k x	
	 -> let tt2		= slurpWitnessKind tt k
		(tt3, x')	= floatBindsX level share tt2 x
	    in	(tt3, XLAM b k x')

	-- check for constant regions on the way down
	XLocal v vts x
	 -> let tt2	= foldl' slurpWitnessKind tt 
	 		$ map (kindOfType . snd) vts
		
		(tt3, x')	= floatBindsX level share tt2 x
	    in	(tt3, XLocal v vts x')
	
	-- When we hit a variable, check to see if there is a binding for it in the table
	XVar v_ t
	 -> let v	= sinkVar share v_
	    in  case Map.lookup v (tableBinds tt) of
		 	Just (x, _)	
			 -> let tt2	= tt { tableBinds = Map.delete v (tableBinds tt) }
			    in	floatBindsX level share tt2 x
	
			Nothing		-> (tt, XVar v t)


	XLam v t x eff clo	
	 -> let -- increase the level number when we pass through a value lambda
		level'	= level + 1
		
		-- don't carry shared expressions into lambdas.. don't want to change the closure properties
		share'	= shareZero

		-- we're carrying a value-var substitution into a lambda expression, so we need
		--	to update the vars in XFree's in the closure annotation
		clo'	= substituteVV (shareVarSub share) clo

		(tt', x')	= floatBindsX level' share' tt x
	    in  (tt', XLam v t x' eff clo')

	XDo{}			-> floatBindsXDo level share tt xx

	XMatch alts		
	 -> let	(tt', alts')	= mapAccumL (floatBindsA level share) tt alts
	    in  (tt', XMatch alts')
	    
	XAPP x t
	 -> let (tt', x')	= floatBindsX level share tt x
	    in	(tt', XAPP x' t)

	XTet vts x
	 -> let (tt', x')	= floatBindsX level share tt x
	    in  (tt', XTet vts x')
 
	XTau t x
	 -> let (tt', x')	= floatBindsX level share tt x
	    in	(tt', XTau t x')


	XApp x1 x2 eff
	 -> let (tt2, x1')	= floatBindsX level share tt  x1
	        (tt3, x2')	= floatBindsX level share tt2 x2
	    in	(tt3, XApp x1' x2' eff)

	XPrim p xs
	 -> let (tt', xs')	= mapAccumL (floatBindsX level share) tt xs
	    in	(tt', XPrim p xs')

	XLit{}			-> (tt, xx)
	XType{}			-> (tt, xx)


-- Alt ---------------------------------------------------------------------------------------------
floatBindsA :: Level -> Share -> Table -> Alt -> (Table, Alt)
floatBindsA level share tt (AAlt gs x)
 = let	(tt2, gs')	= mapAccumL (floatBindsG level share) tt gs
 	(tt3, x')	= floatBindsX level share tt2 x
   in	(tt3, AAlt gs' x')
   

-- Guard -------------------------------------------------------------------------------------------
floatBindsG :: Level -> Share -> Table -> Guard -> (Table, Guard)
floatBindsG level share tt (GExp ws x)
 = let	(tt', x')	= floatBindsX level share tt x
   in	(tt', GExp ws x')


-- inline bindings in an XDo expression
--	walk forwards across the statements
--	Pick all bindings that aren't marked as NoDups
--	Before each statement, drop any carried bindings that conflict with its visible effects
--	
-- TODO: The effects of bindings in the program being walked over are not cached.
--	 If we have a deeply nested program, then the time it takes to recalculate these effects
--	 might start to hurt. Rejig Core.Recon to drop XTau to remember these.

floatBindsXDo :: Level -> Share -> Table -> Exp -> (Table, Exp)
floatBindsXDo level share tt xx@(XDo ss)
 = let	(table', ss')	= floatBindsSS level share tt [] ss
   in	(table', XDo ss')


-- float bindings in a list of statements
--	carrying shared expressions forward and down

floatBindsSS :: Level -> Share -> Table -> [Stmt] -> [Stmt] -> (Table, [Stmt])

floatBindsSS level share table ssAcc [] = (table, ssAcc)

floatBindsSS level share table ssAcc (s : ssRest)
 = let	(share1, table1, ssMore) = floatBindsS level share table s
   in	floatBindsSS level share1 table1 (ssAcc ++ ssMore) ssRest


-- Stmt --------------------------------------------------------------------------------------------
floatBindsS :: Level -> Share -> Table -> Stmt -> (Share, Table, [Stmt])

-- when we get to the end of the block of statements
--	the table should contain no more bindings.
--	We're only inlining things that have exactly one occurance.
--
--	CAREFUL: don't loose or duplicate bindings with top level effects.

-- not a binding
--	!! drop bindings that conflict
floatBindsS level share table ss@(SBind Nothing x)
 = let	(table', x')	= floatBindsX level share table x
   in	(share, table', [SBind Nothing x'])
   

floatBindsS level share table_ ss@(SBind (Just vBind) xBind_)

	----- force 
	-- remember forcings that we haven't seen before
	-- hrm.. this leaves them where they are and never carries them down into match stmts..
	| XPrim MForce [XVar v2 _]	<- xBind
	, Nothing		<- Map.lookup v2 (shareForcings share)
	= let	share'		= share { shareForcings = Map.insert v2 vBind (shareForcings share) }
	  in	(share', table, [SBind (Just vBind) xBind])
	  
	-- replace calls to force with ones that we've seen before
	| XPrim MForce [XVar v2 t]	<- xBind
	, Just v3	<- Map.lookup v2 (shareForcings share)
	= let 	share'	= share { shareVarSub   = Map.insert vBind v3 (shareVarSub share) }
		table'	= (tableStats_ ## statsSharedForcings_ <#> \s -> vBind : s) table	
	  in	(share', table', [])
	  
	----- unbox
	-- remember unboxings we haven't seen before
	| XPrim MUnbox [XType (TVar KRegion vR), XVar v2 _]	<- xBind
	, Nothing	 <- Map.lookup v2 (shareUnboxings share)
	, Set.member vR (tableConstRegions table) -- only move pure unboxings for now
	= let	share'		= share { shareUnboxings = Map.insert v2 (vBind, vR) (shareUnboxings share) }
	  in	(share', table, [SBind (Just vBind) xBind])
	  

	-- replace calls to unbox with ones we've seen before
	| XPrim MUnbox [XType (TVar KRegion vR1), XVar v2 t]	<- xBind
	, Just (v3, vR2) <- Map.lookup v2 (shareUnboxings share)
	, vR1 == vR2
	= let	share'		= share { shareVarSub	= Map.insert vBind v3 (shareVarSub share) }
		table'	= (tableStats_ ## statsSharedUnboxings_ <#> \s -> vBind : s) table	
	  in	(share', table', [])


	  
	-- some other expression
	| otherwise
	= let	-- work out the effects of the expression
		effBind		= t4_3 $ Recon.reconX' (stage ++ ".floatBindsS") xBind

		-- reduce the effect using information about what things are const / pure
		effReduced	= reduceEffect 
					(tableConstRegions table)
					(tableConstTypes table)
					(tablePureEffects table)
					effBind

		-- lookup how this binding is used
		(mUse :: Maybe [Use])	
			= Map.lookup vBind (tableBoundUse table)

		-- decide whether its ok to move it
		(canMove, tt2)	= shouldMove level table mUse vBind effReduced

	   	xx	-- if we're ok to move this binding
			| canMove
			= let	xStripped = stripXTau xBind

				-- add the statement to the table
				tt3	= tt2 { tableBinds = Map.insert vBind (xStripped, effReduced) (tableBinds tt2) }
		
			  in {- trace	( "floatBindsS: picking up binding\n"
		  			% "    ss      = " % ss 	% "\n"
					% "    effBind = " % effBind	% "\n")-}
					(share, tt3, [])

			-- not ok to move this binding
			| otherwise
			= (share, table, [SBind (Just vBind) xBind])
	   in	xx

	-- do the initial decent into the rhs
	where	(table, xBind)	= floatBindsX level share table_ xBind_
	

-- | Strip any XTaus of the front of an expression
stripXTau :: Exp -> Exp
stripXTau (XTau t x)	= stripXTau x
stripXTau xx		= xx


-- shouldMove --------------------------------------------------------------------------------------
-- | Decide whether we should move a binding.
--	If no,  returns (False, Nothing)
--	If yes, returns (True,  Just <expr effect>)
--
shouldMove :: Level -> Table -> Maybe [Use] -> Var -> Effect -> (Bool, Table)
shouldMove level tt mUses vBind effBind
 = let tt'	= (tableStats_ ## statsTotalBindings_ <#> \x -> x + 1) tt
   in  shouldMove' level tt' mUses vBind effBind

shouldMove' level tt mUses vBind effBind

	-- don't move bindings which are set as no-float in the table
	| Set.member vBind (tableNoFloat tt)
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedNoFloat_ <#> \x -> vBind : x)

	-- don't move bindings where there are more than one use
	| Just uses	<- mUses
	, (_:_:_)	<- uses
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedMultiUse_ <#> \x -> vBind : x)


	-- don't move a binding if its use is at a deeper lambda level than here.
	--	* we don't want to risk duplicating work, and it changes the closure term on the lambda.
	--	* can't push an effectful bindings into a lambda, either.
	| Just uses	<- mUses
	, maxUseLevel	<- maximum $ map useLevel uses
	, maxUseLevel > level
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedDeepLambda_ <#> \x -> vBind : x)


	-- don't move bindings that have top level effects.
	--	If we do this we risk moving it /after/ a binding that takes a long time (or doesn't terminate)
	--	This will change the behavior of the program that is visible to the outside world.

	-- TODO: reduce this to just top-level effects
	| effBind /= TBot KEffect 
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedTopLevel_ <#> \x -> vBind : x)


	-- don't move a binding if it has a write or top-level effect, and its use is at a deeper match level than here.
	--	* we can't reduce the number of times these effects are caused.
	--	* reducing the number of read effects is ok (and good!) though.

	-- don't move bindings with no uses, but emit a warning
	| Nothing	<- mUses
	= warning stage
		("shouldMove: binding " % vBind % " has no uses\n")
		(shouldMove_fail tt mUses vBind
			(tableStats_ ## statsNotMovedNoUses_ <#> \x -> vBind : x))
		
	-- all good	
	| otherwise
	= ( True
	  , (tableStats_ ## statsMoved_ <#> \x -> vBind : x) tt)

shouldMove_fail tt mUses vBind statFun
	| Nothing	<- mUses
	= (False, statFun tt)
	
	-- check for missed unboxing opportunities
	| Just uses	<- mUses
	, length [l | UseUnbox l <- uses] == length uses
	= trace ("shouldMove_fail: missed unboxing on " % vBind % "\n")
		$  ( False
		   , (tableStats_ ## statsMissedUnboxing_ <#> \x -> vBind : x) $ statFun tt)

	| otherwise
	= ( False, statFun tt)

-- | Use information about what regions are const and what effects are pure to reduce this effect.
reduceEffect 
	:: Set Var	-- regions known to be constant.
	-> Set Var	-- types known to be deep constant.
	-> Set Var	-- effect vars known to be pure
	-> Effect -> Effect
	
reduceEffect rsConst tsConst esPure eff
 = let	eff'	= makeTSum KEffect 
		$ map (reduceEffect1 rsConst tsConst esPure)
		$ flattenTSum eff

   in {- trace 	( "reduceEffect\n"
  		% "    eff     = " % eff 	% "\n"
  		% "    eff'    = " % eff'	% "\n") $ -}
		eff'

reduceEffect1 rsConst tsConst esPure eff

	-- reduce reads from known constant regions
	| TEffect v [TVar KRegion r]	<- eff
	, v == primRead
	, Set.member r rsConst
	= TBot KEffect
	
	-- reduce reads from known constant types
	| TEffect v [TVar KData t]	<- eff
	, v == primReadT
	, Set.member t rsConst
	= TBot KEffect

	-- reduce known pure effect variables
 	| TVar KEffect v		<- eff
	, Set.member v esPure
	= TBot KEffect

	-- leave it 
	| otherwise
	= eff


-- | If this is a witness to constness of a region or type, or purity of an effect
--	then slurp it into the table.
slurpWitnessKind 
	:: Table -> Kind -> Table
	
slurpWitnessKind tt kk
 = case kk of
	-- const regions
 	KClass v [TVar KRegion r]
	 |  v == primConst	
	 -> tt { tableConstRegions 
	 		= Set.insert r (tableConstRegions tt)}
	
	-- const types
	KClass v [TVar KData t]
	 | v == primConstT 
	 -> tt { tableConstTypes
	 		= Set.insert t (tableConstTypes tt)}

	-- pure effects
	KClass v [TVar KEffect e]
	 |  v == primPure
	 -> tt { tablePureEffects
	 		= Set.insert e (tablePureEffects tt) }

	-- not an interesting kind
	_	-> tt




