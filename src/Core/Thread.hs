
module Core.Thread 
	(threadTree)
	
where

import Core.Exp
import Core.Plate.Walk

import Control.Monad.State
import qualified Data.Map	as Map
import Data.Map			(Map)


-- Maintain a map of regions, to what witnesses we have available for them.
type ThreadS
	= Map 	Var		-- region variable
		[(Var, Var)]	-- class name, witness variable for this class

type ThreadM 
	= State ThreadS


threadTree :: Tree -> Tree
threadTree tree 
	= evalState (walkZM walkTable tree) Map.empty

walkTable 
	= walkTableId
	{ bindK		= thread_bindK 
	, transX 	= thread_bindX }
	
	
thread_bindK :: WalkTable ThreadM -> Var -> Kind -> ThreadM (WalkTable ThreadM)
thread_bindK table v k
	= return table
	
	
thread_bindX :: WalkTable ThreadM -> Exp -> ThreadM Exp
thread_bindX table x
 	= return x

	
