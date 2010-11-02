{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONs -XNoMonomorphismRestriction #-}

-- | The Core simplifier.
--   Rewrites core programs so they are (hopefully) smaller and faster.
module DDC.Core.Simplify
	( Stats(..)
	, simplifyGlob )
where
import Core.Plate.Trans
import DDC.Core.Simplify.Boxing
import DDC.Core.Simplify.Match
import DDC.Core.Simplify.Stats
import DDC.Core.Glob
import Util
import qualified Core.Float	as Float
import qualified Core.Snip	as Snip

-- | Simplify the bindings in a glob.
simplifyGlob
	:: String		-- ^ Unique id for allocating fresh variables.
	-> Glob			-- ^ Header Glob.
	-> Glob			-- ^ Module Glob.
	-> ( Glob		--   Module glob after simplificaton.
	   , [Stats])		--   Stats from each stage of the simplifier
	   
simplifyGlob unique cgHeader cgModule
 = let	(psSimplified, statss)	
 		= simplifyFix 0 unique [] cgHeader cgModule
    in	( psSimplified
   	, statss)


-- | Keep doing passes of the core simplifier until we stop making progress, 
--   that is, until we reach a fix-point.
simplifyFix
	:: Int			-- ^ Cycle count so far.
	-> String 		-- ^ Unique id for allocating fresh variables.
	-> [Stats]		-- ^ Stat accumulator
	-> Glob			-- ^ Header Glob.
	-> Glob			-- ^ Module glob to simplify.
	-> ( Glob		--   Module glob after simplification.
	   , [Stats])		--   Stats from each stage of the simplifier.

simplifyFix cycles unique accStats cgHeader cgModule 
 = let	(cgModule', stats)	
		= simplifyPassAll
			(unique ++ show cycles ++ "p") 
			cgHeader cgModule

   in	if statsProgress stats
   		then simplifyFix (cycles + 1) unique (accStats ++ [stats]) 
			cgHeader cgModule'

		else (cgModule', accStats)


-- | Do a pass of the simplifier
simplifyPassAll
	:: String 		-- ^ Unique id for allocating fresh variables.
	-> Glob			-- ^ Header Glob.
	-> Glob 		-- ^ Module glob to simplify.
	-> ( Glob		--   Module glob after simplification.
           , Stats)		--   Simplifier stats for this pass.

simplifyPassAll unique cgHeader cgModule
 = let	
	-- Extract a table of how many times each binding was used.
	(table', cgFloat)	
		= Float.floatBindsUseOfGlob cgModule
 
	-- All available rules.
	transXM
	 = 	simplifyUnboxBoxX countBoxing 
	 >=>	simplifyMatchX    countMatch  True

	-- Run simplification rules.
	(cgRules, ruleCounts)
	  = runState 
		(mapBindsOfGlobM (transformXM transXM) cgFloat) 
		ruleCountsZero	
	
	-- | Resnip the tree to get back into a-normal form.
	--   This also breaks up compond expressions which makes it more likely
	--   that let-floating will succeed the next time around.
	snipTable	= Snip.Table
			{ Snip.tableHeaderGlob		= cgHeader
			, Snip.tableModuleGlob		= cgModule
			, Snip.tablePreserveTypes	= True }

   	cgSnipped	= Snip.snipGlob snipTable ("x" ++ unique ++ "S") 
			$ cgRules
	
   in	( cgSnipped
	, Stats	{ statsFloat      = Float.tableStats table'
		, statsRuleCounts = ruleCounts })
		
{-
-- | Do a quick tidy up of the glob that doesn't involve
--   moving bindings around.
simplifyTidy :: Glob -> State RuleCounts Glob
 = let	transXM
	 = 	simplifyMatchX	  countMatch

   in	mapBindsOfGlobM (transformXM transXM) glob
-}
	
		