
module Core.Optimise.Simplify
	( Stats(..)
	, coreSimplifyTree )

where

import qualified Core.Float	as Float
import qualified Core.Snip	as Snip
import Core.Exp
import Core.BoundUse
import Core.Plate.Trans

import qualified Debug.Trace	as Debug
import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarUtil

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util


-----
stage		= "Core.Boxing"
debug		= False
trace ss x
 = if debug 
 	then Debug.trace (pprStr ss) x
	else x

type SimplifyM	= VarGenM

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
	| statsReducedUnboxBox stats > 0			= True
--	| length (Float.statsMoved (statsFloat stats)) > 0	= True
	| otherwise						= False

instance Pretty Stats where
 ppr stats
 	= "Simplify.Stats\n"
	% "    - stats from let-floater:\n"
	%> (ppr (statsFloat stats))
	% "\n"
	% "    - unbox/boxings reduced   : " % (statsReducedUnboxBox stats) % "\n"



-- Simplify ----------------------------------------------------------------------------------------
coreSimplifyTree
	:: String		-- unique
	-> Set Var		-- vars defined at top level
	-> Tree			-- source tree
	-> ( Tree		-- new after rewriting
	   , [Stats])		-- stats from each stage of the simplifier
	   
coreSimplifyTree unique topVars cTree
 = let	(psSimplified, statss)	
 			= simplifyTree 0 unique topVars [] cTree
    in	( psSimplified
   	, statss)

-- Keep doing passes of the core simplifier until we stop making progrees
--	(ie, reach a fix-point)
simplifyTree
	:: Int			-- cycle count
	-> String 		-- unique
	-> Set Var		-- vars defined at top level
	-> [Stats]		-- stat accumulator
	-> Tree			-- tree to simplify
	-> ( Tree		-- simplified tree
	   , [Stats])		-- simplifier stats for each pass

simplifyTree cycle unique topVars accStats tree 
 = let	(tree', stats)	= simplifyPass (unique ++ show cycle ++ "p") topVars tree
   in	if statsProgress stats
   		then simplifyTree (cycle + 1) unique topVars (accStats ++ [stats]) tree'
		else (tree', accStats)

-- Do a pass of the simplifier
simplifyPass
	:: String 		-- unique
	-> Set Var		-- vars defined at top level
	-> Tree 		-- tree to simplify
	-> ( Tree		-- simplified tree
           , Stats)		-- simplifier stats

simplifyPass unique topVars tree
 = let	(table', cFloat)	= Float.floatBindsTreeUse tree
 
	-- Zap pairs of Unbox/Box expressions
   	(cZapped, countZapped)	= runState (transformXM zapUnboxBoxX cFloat) 0
	
	-- Resnip the tree to get back into a-normal form.
	--	this breaking up of compond expressions also separates the different effects
	--	and makes it more likely that let floating will succeed next time around.
	
	snipTable	= Snip.Table
			{ Snip.tableTopVars		= topVars
			, Snip.tablePreserveTypes	= True }

   	psSnipped	= Snip.snipTree snipTable ("x" ++ unique ++ "S") cZapped
	
   in	( psSnipped
	, Stats
		{ statsFloat		= Float.tableStats table'
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
 
 
 
 
