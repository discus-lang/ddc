{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONs -XNoMonomorphismRestriction #-}

-- | The Core simplifier.
--   Rewrites core programs so they are (hopefully) smaller and faster.
module DDC.Core.Simplify
	( Stats(..)
	, fixSimplifierPass
	, simplifyPassTidy
	, simplifyPassAll)
where
import Core.Plate.Trans
import DDC.Core.Simplify.Boxing
import DDC.Core.Simplify.Match
import DDC.Core.Simplify.Stats
import DDC.Core.Glob
import Util
import qualified Core.Float	as Float
import qualified Core.Snip	as Snip

type SimplifierPass
	=  String 		-- ^ Unique id for allocating fresh variables.
	-> Glob			-- ^ Header Glob.
	-> Glob 		-- ^ Module glob to simplify.
	-> ( Glob		--   Module glob after simplification.
	   , Stats)		--   Simplifier stats for this pass.


-- | Keep doing passes of the core simplifier until we stop making progress, 
--   that is, until we reach a fix-point.
fixSimplifierPass 
	:: SimplifierPass	-- ^ The pass to run.
	-> String		-- ^ Unique id for generating fresh variables.
	-> Glob 		-- ^ Header glob
	-> Glob			-- ^ Module glob
	-> (Glob, [Stats])	
	
fixSimplifierPass pass unique cgHeader cgModule_
 = go 0 [] cgModule_
 where	go (cycles :: Int) accStats cgModule
	 = let	(cgModule', stats) 
			= pass 	(unique ++ show cycles ++ "p")
				cgHeader cgModule

	   in	if statsProgress stats
   		 then go (cycles + 1) (accStats ++ [stats]) cgModule'
		 else (cgModule', accStats)


-- | Do a quick tidy up that doesn't involve moving bindings around.
simplifyPassTidy :: SimplifierPass
simplifyPassTidy _unique _cgHeader cgModule
 = let	transXM
	 = 	simplifyMatchX	  countMatch False
	
	(cgFinal, ruleCounts) 
	 = runState 	(mapBindsOfGlobM (transformXM transXM) cgModule) 
			ruleCountsZero
			
   in	( cgFinal
	, Stats	{ statsFloat		= Nothing
		, statsRuleCounts	= ruleCounts })
						

-- | Do a pass of the simplifier
simplifyPassAll :: SimplifierPass 
simplifyPassAll unique cgHeader cgModule
 = let	
	-- Extract a table of how many times each binding was used.
	(table', cgFloat)	
		= Float.floatBindsUseOfGlob cgModule
 
	-- All available rules.
	table
	 = transTableId
	 { transX	=  	simplifyUnboxBoxX countBoxing 
	 		>=>	simplifyMatchX    countMatch  True

	 , transA	=	simplifyMatchA    countMatch }
 
	-- Run simplification rules.
	(cgRules, ruleCounts)
	  = runState 
		(mapBindsOfGlobM (transZM table) cgFloat) 
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
	, Stats	{ statsFloat      = Just $ Float.tableStats table'
		, statsRuleCounts = ruleCounts })
			
		