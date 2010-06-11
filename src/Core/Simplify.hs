
module Core.Simplify
	( Stats(..)
	, simplifyGlob )
where
import Core.Plate.Trans
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Core.Glob
import Util
import qualified Core.Float	as Float
import qualified Core.Snip	as Snip


-- Stats -------------------------------------------------------------------------------------------
-- stats about what happened during a pass of the simplifier
data	Stats
	= Stats
	{ -- stats from the let-floating
	  statsFloat			:: Float.Stats

	  -- number of unbox/box pairs eliminated
	, statsReducedUnboxBox		:: Int }


-- | check the stats of a simplifier pass to see if we made any progress
statsProgress :: Stats -> Bool
statsProgress stats
	| statsReducedUnboxBox stats > 0				= True
	| not $ isNil $ Float.statsSharedForcings  $ statsFloat stats	= True
	| not $ isNil $ Float.statsSharedUnboxings $ statsFloat stats	= True
	| otherwise							= False


instance Pretty Stats PMode where
 ppr stats
 	= "Simplify.Stats\n"
	% "    - stats from let-floater:\n"
	%> (ppr (statsFloat stats))
	% "\n"
	% "    - unbox/boxings reduced   : " % (statsReducedUnboxBox stats) % "\n"



-- Simplify ----------------------------------------------------------------------------------------
simplifyGlob
	:: String		-- unique
	-> Glob			-- Header Glob.
	-> Glob			-- Module Glob.
	-> ( Glob		-- new after rewriting
	   , [Stats])		-- stats from each stage of the simplifier
	   
simplifyGlob unique cgHeader cgModule
 = let	(psSimplified, statss)	
 		= simplifyFix 0 unique [] cgHeader cgModule
    in	( psSimplified
   	, statss)


-- Keep doing passes of the core simplifier until we stop making progrees
--	(ie, reach a fix-point)
simplifyFix
	:: Int			-- cycle count
	-> String 		-- unique
	-> [Stats]		-- stat accumulator
	-> Glob			-- Header Glob.
	-> Glob			-- glob to simplify
	-> ( Glob		-- simplified glob.
	   , [Stats])		-- simplifier stats for each pass

simplifyFix cycle unique accStats cgHeader cgModule 
 = let	(cgModule', stats)	
		= simplifyPass 
			(unique ++ show cycle ++ "p") 
			cgHeader cgModule

   in	if statsProgress stats
   		then simplifyFix (cycle + 1) unique (accStats ++ [stats]) 
			cgHeader cgModule'

		else (cgModule', accStats)


-- Do a pass of the simplifier
simplifyPass
	:: String 		-- unique
	-> Glob			-- Header glob.
	-> Glob 		-- Module glob to simplify.
	-> ( Glob		-- simplified glob
           , Stats)		-- simplifier stats

simplifyPass unique cgHeader cgModule
 = let	(table', cgFloat)	= Float.floatBindsUseOfGlob cgModule
 
	-- Zap pairs of Unbox/Box expressions
   	(cgZapped, countZapped)
		= runState 
			(mapBindsOfGlobM (transformXM zapUnboxBoxX) cgFloat)
			0
	
	-- Resnip the tree to get back into a-normal form.
	--	this breaking up of compond expressions also separates the different effects
	--	and makes it more likely that let floating will succeed next time around.
	
	snipTable	= Snip.Table
			{ Snip.tableHeaderGlob		= cgHeader
			, Snip.tableModuleGlob		= cgModule
			, Snip.tablePreserveTypes	= True }

   	cgSnipped	= Snip.snipGlob snipTable ("x" ++ unique ++ "S") 
			$ cgZapped
	
   in	( cgSnipped
	, Stats	{ statsFloat		= Float.tableStats table'
		, statsReducedUnboxBox	= countZapped })
		

-- Simply an expression 
zapUnboxBoxX :: Exp -> State Int Exp
zapUnboxBoxX xx
 = case xx of 

	-- unbox/box
 	XPrim MUnbox [r1, XPrim MBox [r2, x]]
		| r1 == r2	
		-> do	modify $ \s -> s + 1
			return	x
		
	-- unbox/force/box
	XPrim MUnbox [r1, XPrim MForce [XPrim MBox [r2, x]]]
		| r1 == r2
		-> do	modify $ \s -> s + 1
			return x
		
	_	-> return xx

