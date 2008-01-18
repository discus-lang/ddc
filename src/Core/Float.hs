
module Core.Float
	( Table(..)
	, tableZero
	, floatBindsTree)

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
 	then Debug.trace (pprStr ss) x
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
	}							


-- empty inliner table
tableZero
	= Table
	{ tableNoFloat		= Set.empty
	, tableBoundUse		= Map.empty
	, tableConstRegions	= Set.empty
	, tableConstTypes	= Set.empty
	, tablePureEffects	= Set.empty
	, tableBinds		= Map.empty }
	

-- floatBinds -------------------------------------------------------------------------------------

-- tree
floatBindsTree 
	:: Table -> Tree -> (Table, Tree)

floatBindsTree tt tree
	= mapAccumL (floatBindsP 0) tt tree
   

-- top
floatBindsP :: Level -> Table -> Top -> (Table, Top)
floatBindsP level tt pp
 = case pp of
 	PBind v x	
	 -> let	(tt', x')	= floatBindsX level tt x
	    in	(tt', PBind v x')

	_		-> (tt, pp)
	
-- exp
floatBindsX :: Level -> Table -> Exp -> (Table, Exp)
floatBindsX level tt xx
 = case xx of

	-- check for constant regions on the way down
 	XLAM b k x	
	 -> let tt2		= slurpWitnessKind tt k
		(tt3, x')	= floatBindsX level tt2 x
	    in	(tt3, XLAM b k x')

	-- check for constant regions on the way down
	XLocal v vts x
	 -> let tt2	= foldl' slurpWitnessKind tt 
	 		$ map (kindOfType . snd) vts
		
		(tt3, x')	= floatBindsX level tt2 x
	    in	(tt3, XLocal v vts x')
	
	-- When we hit a variable, check to see if there is a binding for it in the table
	XVar v t
	 -> case Map.lookup v (tableBinds tt) of
	 	Just (x, _)	
		 -> let tt2	= tt { tableBinds = Map.delete v (tableBinds tt) }
		    in	floatBindsX level tt2 x

		Nothing		-> (tt, xx)

	-- increase the level number when we pass through a value lambda
	XLam v t x eff clo	
	 -> let (tt', x')	= floatBindsX (level + 1) tt x
	    in  (tt', XLam v t x' eff clo)


	XDo{}			-> floatBindsXDo level tt xx

	XMatch alts		
	 -> let	(tt', alts')	= mapAccumL (floatBindsA level) tt alts
	    in  (tt', XMatch alts')
	    
	XAPP x t
	 -> let (tt', x')	= floatBindsX level tt x
	    in	(tt', XAPP x' t)

	XTet vts x
	 -> let (tt', x')	= floatBindsX level tt x
	    in  (tt', XTet vts x')
 
	XTau t x
	 -> let (tt', x')	= floatBindsX level tt x
	    in	(tt', XTau t x')


	XApp x1 x2 eff
	 -> let (tt2, x1')	= floatBindsX level tt  x1
	        (tt3, x2')	= floatBindsX level tt2 x2
	    in	(tt3, XApp x1' x2' eff)

	XPrim p xs
	 -> let (tt', xs')	= mapAccumL (floatBindsX level) tt xs
	    in	(tt', XPrim p xs')

	XLit{}			-> (tt, xx)
	XType{}			-> (tt, xx)


-- alts
floatBindsA :: Level -> Table -> Alt -> (Table, Alt)
floatBindsA level tt (AAlt gs x)
 = let	(tt2, gs')	= mapAccumL (floatBindsG level) tt gs
 	(tt3, x')	= floatBindsX level tt2 x
   in	(tt3, AAlt gs' x')
   

-- guards
floatBindsG :: Level -> Table -> Guard -> (Table, Guard)
floatBindsG level tt (GExp ws x)
 = let	(tt', x')	= floatBindsX level tt x
   in	(tt', GExp ws x')


-- inline bindings in an XDo expression
--	walk forwards across the statements
--	Pick all bindings that aren't marked as NoDups
--	Before each statement, drop any carried bindings that conflict with its visible effects
--	
-- TODO: The effects of bindings in the program being walked over are not cached.
--	 If we have a deeply nested program, then the time it takes to recalculate these effects
--	 might start to hurt. Rejig Core.Recon to drop XTau to remember these.

floatBindsXDo :: Level -> Table -> Exp -> (Table, Exp)
floatBindsXDo level tt xx@(XDo ss)
 = let	(tt', sssNew)	= mapAccumL (floatBindsS level) tt ss
   in	( tt', XDo (concat sssNew) )
	

floatBindsS :: Level -> Table -> Stmt -> (Table, [Stmt])

-- when we get to the end of the block of statements
--	the table should contain no more bindings.
--	We're only inlining things that have exactly one occurance.
--
--	CAREFUL: don't loose or duplicate bindings with top level effects.

-- not a binding
--	!! drop bindings that conflict
floatBindsS level tt ss@(SBind Nothing x)
 = let	(tt', x')	= floatBindsX level tt x
   in	(tt', [SBind Nothing x'])
   

floatBindsS level tt ss@(SBind (Just vBind) xBind)
	-- work out the effects of the expression
 = let	effBind	= t4_3 $ Recon.reconX' (stage ++ ".floatBindsS") xBind
	mUse	= Map.lookup vBind (tableBoundUse tt)

   	xx	-- if we're ok to move this binding
		| shouldMove level tt mUse vBind effBind
		= let	xStripped = stripXTau xBind

			-- add the statement to the table
			tt'	= tt { tableBinds = Map.insert vBind (xStripped, effBind) (tableBinds tt) }
		
		  in	trace	( "floatBindsS: picking up binding\n"
	  			% "    ss      = " % ss 	% "\n"
				% "    effBind = " % effBind	% "\n")
				(tt', [])

		-- not ok to move this binding
		| otherwise
		= let (tt', x')	= floatBindsX level tt xBind
		  in  (tt', [SBind (Just vBind) x'])
   in	xx


-- | Decide whether we should move a binding.
--	If no,  returns (False, Nothing)
--	If yes, returns (True,  Just <expr effect>)
--
shouldMove :: Level -> Table -> Maybe [Use] -> Var -> Effect -> Bool
shouldMove level tt mUses vBind effBind

	-- don't move bindings which are set as no-float in the table
	| Set.member vBind (tableNoFloat tt)
	= False
	
	-- don't move bindings who's effects are not pure
	--	TODO: relax this
	| effBind /= TBot KEffect 
	= False

	-- don't move bindings with no uses, but emit a warning
	| Nothing	<- mUses
	= warning stage
		("shouldMove: binding " % vBind % " has no uses\n")
		$ False

	-- don't move bindings where there are more than one use
	| Just uses	<- mUses
	, (_:_:_)	<- uses
	= False

	-- don't move a binding if its use is at a deeper lambda level than here.
	--	* we don't want to risk duplicating work, and it changes the closure term on the lambda.
	--	* can't push an effectful bindings into a lambda, either.
	| Just uses	<- mUses
	, maxUseLevel	<- maximum $ map useLevel uses
	, maxUseLevel > level
	= False

	-- don't move a binding if it has a write or top-level effect, and its use is at a deeper match level than here.
	--	* we can't reduce the number of times these effects are caused.
	--	* reducing the number of read effects is ok (and good!) though.


	-- all good	
	| otherwise
	= True

-- | Strip any XTaus of the front of an expression
stripXTau :: Exp -> Exp
stripXTau (XTau t x)	= stripXTau x
stripXTau xx		= xx


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
	 		= Set.insert v (tableConstRegions tt)}
	
	-- const types
	KClass v [TVar KData t]
	 | v == primConstT 
	 -> tt { tableConstTypes
	 		= Set.insert v (tableConstTypes tt)}

	-- pure effects
	KClass v [TVar KEffect e]
	 |  v == primPure
	 -> tt { tablePureEffects
	 		= Set.insert v (tablePureEffects tt) }

	-- not an interesting kind
	_	-> tt




