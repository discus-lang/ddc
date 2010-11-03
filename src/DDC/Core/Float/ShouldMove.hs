{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Float.ShouldMove
	(shouldMove)
where
import Core.BoundUse
import Core.Util.Bits
import DDC.Var
import DDC.Type
import DDC.Core.Float.Table
import DDC.Core.Float.Stats
import DDC.Core.Exp
import DDC.Main.Error
import DDC.Main.Pretty
import Shared.Warning
import qualified Debug.Trace	as Debug
import qualified Data.Set	as Set
import Util

stage		= "DDC.Core.Float.ShouldMove"
debug		= False
trace ss x	= if debug then Debug.trace (pprStrPlain (stage % ":" % ss)) x else x

-- | Decide whether we should move a binding.
--	If no,  returns (False, Nothing)
--	If yes, returns (True,  Just <expr effect>)
shouldMove 
	:: Level 		-- ^ Number of enclosing lambdas.
	-> Table 		-- ^ Table containing set of bindings not to move.
	-> Maybe [Use] 		-- ^ Number of uses for this binding.
	-> Var 			-- ^ The bound variable.
	-> Effect 		-- ^ The effect of the right of the binding.
	-> Exp			-- ^ The right of the binding
	-> (Bool, Table)	-- ^ Whether we should move it, and updated table saying why.

shouldMove level tt mUses vBind effBind xx
 = let tt'	= (tableStats_ ## statsTotalBindings_ <#> \x -> x + 1) tt
   in  shouldMove' level tt' mUses vBind effBind xx

shouldMove' level tt mUses vBind effBind xx

	-- Don't move bindings which are set as no-float in the table
	| Set.member vBind (tableNoFloat tt)
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedNoFloat_ <#> \x -> vBind : x)

	-- Don't move bindings that have more than one use.
	--	* we don't want to duplicate work.
	--	* can't duplicate effectful bindings.
	| Just uses	<- mUses
	, _ : _ :_	<- uses
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedMultiUse_ <#> \x -> vBind : x)

	-- Don't move a binding if has a use at a deeper lambda level than here.
	-- 	* we don't want to risk duplicating work, and it changes the closure term on the lambda.
	-- 	* can't push an effectful bindings into a lambda, either.
	| Just uses	<- mUses
	, maxUseLevel	<- maximum $ map useLevel uses
	, maxUseLevel > level
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedDeepLambda_ <#> \x -> vBind : x)

	-- Don't move bindings that have unmaskable effects.
	-- TODO: Maybe we should still move them closer to their use sides, just not across
	--       bindings with other interferring effects.
	| effBind /= tPure 
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedEffects_ <#> \x -> vBind : x)

	-- Don't move bindings with no uses, but emit a warning
	| Nothing	<- mUses
	= warning (WarnUselessBinding vBind)
		(shouldMove_fail tt mUses vBind
			(tableStats_ ## statsNotMovedNoUses_ <#> \x -> vBind : x))
	
	-- Don't move compound things.
	-- TODO: We should really do this, but Core.Prep doesn't handle it yet.
	| xx'	<- stripXTau xx
	, isXLambda xx' || isXLAMBDA xx' || isXMatch xx' || isXLocal xx'
	= shouldMove_fail tt mUses vBind id
		
	-- All good, shift it!
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
