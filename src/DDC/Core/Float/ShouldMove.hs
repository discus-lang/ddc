{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Float.ShouldMove
	(shouldMove)
where
import Core.BoundUse
import DDC.Var
import DDC.Type
import DDC.Core.Float.Table
import DDC.Core.Float.Stats
import DDC.Main.Error
import DDC.Main.Pretty
import Shared.Warning
import qualified Debug.Trace	as Debug
import qualified Data.Set	as Set
import Util

stage		= "DDC.Core.Float.ShouldMove"
debug		= False
trace ss x	= if debug then Debug.trace (pprStrPlain (stage % ":" % ss)) x else x

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
	| effBind /= tPure 
	= shouldMove_fail tt mUses vBind
		(tableStats_ ## statsNotMovedTopLevel_ <#> \x -> vBind : x)


	-- don't move a binding if it has a write or top-level effect, and its use is at a deeper match level than here.
	--	* we can't reduce the number of times these effects are caused.
	--	* reducing the number of read effects is ok (and good!) though.

	-- don't move bindings with no uses, but emit a warning
	| Nothing	<- mUses
	= warning (WarnUselessBinding vBind)
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

