
module Type.Trace.TraceFetters
(
	traceFettersZ
)

where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Util.Data.Bag	as Bag
import Util.Data.Bag		(Bag)

import Type.Exp
import Type.Util
import Type.Plate
import Type.State
import Type.Class


-----------------------
-- traceFettersZ
--	Find all fetters reachable by tracing back from this class.
--
traceFettersZ 
	:: Set ClassId			-- ^ Classes visited so far.
	-> Bag ClassId			-- ^ Classes still to visit.
	-> [(ClassId, Fetter)]		-- ^ Fetters found so far.
	-> SquidM [(ClassId, Fetter)]
	
traceFettersZ visited (Bag.Nil) acc	
	= return acc

traceFettersZ visited more acc
 =  case Bag.mustTake1 more of
     (# cidZ, more' #)	
      -> if Set.member cidZ visited
      		then traceFettersZ visited more' acc
		
		else do	Just c	<- lookupClass cidZ
	 		case classKind c of
	 			KFetter	
				 -> do	let fs		= [(\(TFetter f) -> f) $ classType c]
					fs'		<- mapM updateVC fs
					let acc'	= [(cidZ, f) | f <- fs']

				 	traceFettersZ (Set.insert cidZ visited) more' (acc' ++ acc)

				KClosure 	
				 -> 	traceFettersZ (Set.insert cidZ visited) more' acc

				_ -> 	traceFettersZ 	(Set.insert cidZ visited)
							((Bag.fromList $ Set.toList $ classBackRef c)
							
								`Bag.union` more)
							acc
	

