{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Float
	( Table(..)
	, tableZero
	, Stats(..)
	, floatBindsOfGlob
	, floatBindsUseOfGlob)
where
import DDC.Core.Float.Util
import DDC.Core.Float.ShouldMove
import DDC.Core.Float.Share
import DDC.Core.Float.Stats
import DDC.Core.Float.Table
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Core.Check
import DDC.Type
import DDC.Var
import DDC.Main.Error
import Core.BoundUse
import Core.Util
import Util
import qualified Data.Map		as Map
import qualified Data.Set		as Set

stage		= "DDC.Core.Float"

-- | Calculate usage information and float the binds in this tree
floatBindsUseOfGlob
	:: Glob 
	-> (Table, Glob)

floatBindsUseOfGlob cgModule
 = let	-- count the number of bound occurances of each variable
 	boundUse	= execState (boundUseGlob cgModule) Map.empty

	-- float bindings into their use sites
	table		= tableZero { tableBoundUse = boundUse }

   in	floatBindsOfGlob table cgModule


-- | Float the binds in this tree
floatBindsOfGlob
	:: Table 
	-> Glob
	-> (Table, Glob)

floatBindsOfGlob table cgModule
 = let	(table', vpsBind')
		= mapAccumL (floatBindsP 0 shareZero) table 
		$ Map.toList 
		$ globBind cgModule

   in	( table'
	, cgModule { globBind = Map.fromList vpsBind' })
   

-- Top ---------------------------------------------------------------------------------------------
floatBindsP 
	:: Level 
	-> Share 
	-> Table 
	-> (Var, Top)
	-> (Table, (Var, Top))

floatBindsP level share table (v1, pp)
 = case pp of
 	PBind v2 x	
	 -> let	(tt', x')	= floatBindsX level share table x
	    in	(tt', (v1, PBind v2 x'))

	_	-> (table, (v1, pp))
	
	
-- Exp ---------------------------------------------------------------------------------------------
floatBindsX :: Level -> Share -> Table -> Exp -> (Table, Exp)
floatBindsX level share tt xx
 = case xx of
	XNil -> (tt, XNil)

	-- check for constant regions on the way down
 	XLAM b k x	
	 -> let tt2		= slurpWitnessKind tt k
		(tt3, x')	= floatBindsX level share tt2 x
	    in	(tt3, XLAM b k x')

	-- check for constant regions on the way down
	XLocal v vts x
	 -> let ks	= map (kindOfType . snd) vts
	 	tt2	= foldl' slurpWitnessKind tt ks
		
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

	XTau t x
	 -> let (tt', x')	= floatBindsX level share tt x
	    in	(tt', XTau t x')


	XApp x1 x2
	 -> let (tt2, x1')	= floatBindsX level share tt  x1
	        (tt3, x2')	= floatBindsX level share tt2 x2
	    in	(tt3, XApp x1' x2')

	XPrim p xs
	 -> let (tt', xs')	= mapAccumL (floatBindsX level share) tt xs
	    in	(tt', XPrim p xs')

	XPrimType{}			-> (tt, xx)

	XLit{}			-> (tt, xx)


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
floatBindsXDo level share tt (XDo ss)
 = let	(table', ss')	= floatBindsSS level share tt [] ss
   in	(table', XDo ss')

floatBindsXDo _ _ _ _
	= panic stage $ "floatBindsXDo: no a do"

-- float bindings in a list of statements
--	carrying shared expressions forward and down

floatBindsSS :: Level -> Share -> Table -> [Stmt] -> [Stmt] -> (Table, [Stmt])

floatBindsSS _ _ table ssAcc [] = (table, ssAcc)

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
floatBindsS level share table (SBind Nothing x)
 = let	(table', x')	= floatBindsX level share table x
   in	(share, table', [SBind Nothing x'])
   

floatBindsS level share table_ (SBind (Just vBind) xBind_)

	----- force 
	-- remember forcings that we haven't seen before
	-- hrm.. this leaves them where they are and never carries them down into match stmts..
	| XPrim MForce [XVar v2 _]	<- xBind
	, Nothing		<- Map.lookup v2 (shareForcings share)
	= let	share'		= share { shareForcings = Map.insert v2 vBind (shareForcings share) }
	  in	(share', table, [SBind (Just vBind) xBind])
	  
	-- replace calls to force with ones that we've seen before
	| XPrim MForce [XVar v2 _]	<- xBind
	, Just v3	<- Map.lookup v2 (shareForcings share)
	= let 	share'	= share { shareVarSub   = Map.insert vBind v3 (shareVarSub share) }
		table'	= (tableStats_ ## statsSharedForcings_ <#> \s -> vBind : s) table	
	  in	(share', table', [])
	  
	----- unbox
	-- remember unboxings we haven't seen before
	| XPrim MUnbox [XPrimType (TVar kR (UVar vR)), XVar v2 _]	<- xBind
	, kR	== kRegion
	, Nothing	 <- Map.lookup v2 (shareUnboxings share)
	, Set.member vR (tableConstRegions table) -- only move pure unboxings for now
	= let	share'		= share { shareUnboxings = Map.insert v2 (vBind, vR) (shareUnboxings share) }
	  in	(share', table, [SBind (Just vBind) xBind])
	  

	-- replace calls to unbox with ones we've seen before
	| XPrim MUnbox [XPrimType (TVar kR (UVar vR1)), XVar v2 _]	<- xBind
	, kR	== kRegion
	, Just (v3, vR2) <- Map.lookup v2 (shareUnboxings share)
	, vR1 == vR2
	= let	share'		= share { shareVarSub	= Map.insert vBind v3 (shareVarSub share) }
		table'	= (tableStats_ ## statsSharedUnboxings_ <#> \s -> vBind : s) table	
	  in	(share', table', [])


	  
	-- some other expression
	| otherwise
	= let	-- work out the effects of the expression
		effBind		= checkedTypeOfOpenExp (stage ++ ".floatBindsS") xBind

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
stripXTau (XTau _ x)	= stripXTau x
stripXTau xx		= xx

