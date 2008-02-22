
module Type.Util.Substitute
	( subTT
	, subTT_cutM
	, subTT_noLoops
	, subTT_all
	, SubM)
where

import Type.Plate.Collect
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

type SubM	= State [(Type, Type)]


-- | Do a loop cutting substitution on this type.
subTT 	:: Map Type Type 	-- ^ types to substitute for.
	-> Type 		-- ^ type to substitute into.
	-> ( Type		-- ^ result type.
	   , [(Type, Type)])	-- ^ list of types from the substitution that were looping

subTT sub tt
 = let	(tt', loops)	= runState (subTT_cutM sub Set.empty tt) []
   in	(tt', loops)


-- | Do a loop cutting substition on this type.
--	panic if there are any loops through the data portion.
--
subTT_noLoops
	:: Map Type Type
	-> Type
	-> Type

subTT_noLoops sub tt
 = let	(tt', loops)	= runState (subTT_cutM sub Set.empty tt) []
   in	if null loops
   		then tt'
		else panic stage
			$ "subTT_noLoops: found loops through data portion of type\n"
			% "  tt            = " % tt		% "\n\n"
			% "  loops through = " % loops		% "\n"

	
-- | Do a loop cutting substitution on this type.
--	Collect any loops in the data portion of the type in the monad.
subTT_cutM 
	:: Map Type Type	-- types to substitute for
	-> Set Type		-- types that have already been substituted, and should be cut
	-> Type 		-- type to substitute for
	-> SubM Type
	
subTT_cutM sub cut tt
 = do	sub'	<- cutSub sub
 	subTT_cutM' sub' cut tt
	
subTT_cutM' sub cut tt
 = let down	= subTT_cutM' sub cut
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

	-- see if we can substitute something
	| Just tt'	<- Map.lookup tt sub
	= if Set.member tt cut
	   then case kindOfType tt of
			-- Loops in effect and closure types can be cut by replacing
			--	the looping variable with bottom.
			KEffect		
			 -> 	return	$ TBot KEffect
		
			KClosure	
			 -> 	return	$ TBot KClosure
		
			-- Loops in other parts of the graph are errors.
			_ -> do
				modify $ \s -> (tt, tt') : s
				return	tt

	   else subTT_cutM' sub (Set.insert tt cut) tt'


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


-- | Check substitution for loops through the data portion of the type
cutSub :: Map Type Type -> SubM (Map Type Type)
cutSub sub
 = do 	
	sub'	<- liftM Map.fromList
		$  liftM catMaybes
		$  mapM cutF
		$  Map.toList sub
		
	return	sub'

cutF :: (Type, Type) -> SubM (Maybe (Type, Type))
cutF (t1, t2)
	-- If the binding var is in the rhs then we've got an infinite type error
	| kindOfType t1 == KData
	= if elem t1 $ collectTClassVars t2
		then do	modify $ \s -> (t1, t2) : s
			return $ Nothing

		else return $ Just (t1, t2)
		
	| otherwise
	= return $ Just (t1, t2)	



