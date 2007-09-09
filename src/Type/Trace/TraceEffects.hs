
module Type.Trace.TraceEffects
(
--	traceEffectsZ	
)

where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Util.Data.Bag	as Bag
import Util.Data.Bag		(Bag)

import Type.Exp
import Type.Util
import Type.State
import Type.Class

-----------------------
-- traceEffectsZ
--	Find all effect classes containing ECons which are reachable
--	by tracing back from this class.
--
{-
traceEffectsZ
	:: Set ClassId 		-- TRE classes already visited
	-> Bag ClassId		-- classes still to visit
	-> [ClassId]		-- ECon classes found so far
	-> SquidM [ClassId]
	
traceEffectsZ visited (Bag.Nil) acc
	= return acc

traceEffectsZ visited more acc
 = case Bag.mustTake1 more of
    (# cidZ, more' #)
     -> if Set.member cidZ visited
	 then traceEffectsZ visited more' acc
	 else do
		Just c	<- lookupClass cidZ
		case classKind c of
			KEffect		-> traceEffectsE  visited cidZ more' acc
			KType		-> traceEffectsTR visited cidZ more' acc
			KRegion		-> traceEffectsTR visited cidZ more' acc
			KClosure	-> traceEffectsZ  (Set.insert cidZ visited) more' acc
			KFetter		-> traceEffectsZ  (Set.insert cidZ visited) more' acc
	

traceEffectsTR visited cidZ more' acc
 = do
 	Just c	<- lookupClass cidZ
	traceEffectsZ
		(Set.insert cidZ visited)
		(more' `Bag.union` classBackRef c)
		acc
	
traceEffectsE visited cidZ more' acc
 = do
 	Just c		<- lookupClass cidZ

	let effs	= case classQueue c of
				[TEffect (ESum es)]	-> es
				_			-> []
	
	let eCons	= filter isECon effs

	if isNil eCons
	 then traceEffectsZ
	  	(Set.insert cidZ visited)
		(more' `Bag.union` classBackRef c)
		acc

 	 else traceEffectsZ 
	  	(Set.insert cidZ visited)
		more'
		(cidZ : acc)
		
-}	
