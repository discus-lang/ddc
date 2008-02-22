
module Type.Util.Substitute
	( subTT
	, subTT_noLoops
	, subTT_all
	, SubM)
where

import Type.Plate.Trans
import Type.Util.Bits
import Type.Exp

import Shared.Error
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

stage	= "Type.Util.Substitute"

-- | Do a loop cutting substition and panic if there are any loops
--	through the data portion of the type.
subTT_noLoops
	:: Map Type Type
	-> Type
	-> Type

subTT_noLoops sub tt
 = let	(tt', loopSet)	= runState (subTT_cutM sub Set.empty tt) Set.empty
   in	if Set.null loopSet
   		then tt'
		else panic stage
			$ "subTT_noLoops: found loops through data portion of type\n"
			% "  tt            = " % tt		% "\n\n"
			% "  loops through = " % loopSet	% "\n"

-- Do a loop cutting substitution on this type.
subTT 	:: Map Type Type 	-- ^ types to substitute for.
	-> Type 		-- ^ type to substitute into.
	-> ( Type		-- ^ result type.
	   , [Type])		-- ^ list of data vars that were looping.

subTT sub tt
 = let	(tt', loopSet)	= runState (subTT_cutM sub Set.empty tt) Set.empty
   in	(tt', Set.toList loopSet)
	
	
type SubM	= State (Set Type)


-- Loop cutting substitution
subTT_cutM 
	:: Map Type Type	-- types to substitute for
	-> Set Type		-- types that have already been substituted, and should be cut
	-> Type 		-- type to substitute for
	-> SubM Type
	
subTT_cutM sub cut tt
 = let down	= subTT_cutM sub cut
   in case tt of
 	TForall vks t		
	 -> do	t'	<- down t
	 	return	$ TForall vks t'

	TFetters fs t		
	 -> do	t'	<- down t
	 	return	$ TFetters fs t'

	TSum k ts
	 -> do	ts'	<- mapM down ts
	 	return	$ TSum k ts'
		
	TMask k t1 t2
	 -> do	t1'	<- down t1
	 	t2'	<- down t2
		return	$ TMask k t1' t2'
		
		
	TTop{}		-> return tt
	TBot{}		-> return tt
	
	TData v ts		
	 -> do	ts'	<- mapM down ts
	 	return	$ TData v ts'
		
	TFun t1 t2 eff clo
	 -> do	t1'	<- down t1
	 	t2'	<- down t2
		eff'	<- down eff
		clo'	<- down clo
		return	$ TFun t1' t2' eff' clo'
	
	TEffect v ts
	 -> do	ts'	<- mapM down ts
	 	return	$ TEffect v ts'

	TFree v t
	 -> do	t'	<- down t
	 	return	$ TFree v t'

	TDanger t1 t2
	 -> do	t1' 	<- down t1
	 	t2'	<- down t2
		return	$ TDanger t1' t2'

	TTag{}		-> return tt
	
	TWild{}		-> return tt

	TError{}	-> return tt

	TVar{} 		-> subTT_enter sub cut tt
	TClass{}	-> subTT_enter sub cut tt
	
subTT_enter sub cut tt

 	-- check for loops
	| Set.member tt cut
	= case kindOfType tt of
		-- Loops in effect and closure types can be cut by replacing
		--	the looping variable with bottom.
		KEffect		
		 -> 	return	$ TBot KEffect
		
		KClosure	
		 -> 	return	$ TBot KClosure
		
		-- Loops in other parts of the graph are errors.
		_ -> do
			modify $ \s -> Set.insert tt s
			return	tt

	-- see if we can substitute something
	| Just tt'	<- Map.lookup tt sub
	= subTT_cutM sub (Set.insert tt cut) tt'
	
	-- nothing can be substituted
	| otherwise
	= return tt
	



-- Substitute types for types everywhere in a type, including binding positions.
--	This is not loop-avoiding.
subTT_all	
	:: TransM (State ()) a
	=> Map Type Type
	-> a -> a

subTT_all sub tt
 	= transformT (\t -> case t of
			TVar{}
			 -> case Map.lookup t sub of
				Just t'	-> t'
				Nothing	-> t
					
			TClass{} 
			 -> case Map.lookup t sub of
			 	Just t'	-> t'
				Nothing	-> t
				
			_	-> t)
	$ tt
