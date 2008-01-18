
module Core.Optimise.Boxing
	( coreBoxingTree )

where

import Core.Float
import Core.Snip
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

type BoxingM	= VarGenM


-----

coreBoxingTree
	:: String	-- unique
	-> Set Var	-- vars defined at top level
	-> Tree		-- source tree
	-> ( Tree	-- new after rewriting
	   , Int)	-- count of how many unbox/box pairs were eliminated
	   
coreBoxingTree unique topVars cTree
 = let	-- count the number of bound occurances of each variable
 	boundUse		= execState (boundUseTree cTree) Map.empty

	-- float bindings into their use sites
	table			= tableZero { tableBoundUse = boundUse }
   	(table', cFloat)	= floatBindsTree table cTree
   
	-- Zap pairs of Unbox/Box expressions
   	(cZapped, countZapped)	= runState (transformXM zapUnboxBoxX cFloat) 0

	-- Resnip the tree to get back into a-normal form.
   	cSnipped	= snipTree topVars ("x" ++ unique ++ "S") cZapped
      
   in 	trace 	( "coreBoxing:\n"
  		% "\n" %!% Map.toList boundUse % "\n") 
		(cSnipped, countZapped)
		 
zapUnboxBoxX :: Exp -> State Int Exp
zapUnboxBoxX xx
 = case xx of 
 	XPrim MUnbox [r1, XPrim MBox [r2, x]]
		| r1 == r2	
		-> do	modify $ \s -> s + 1
			return	x
		
	_	-> return xx
 
 
 
 
