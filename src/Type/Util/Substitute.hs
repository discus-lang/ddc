{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Util.Substitute
	( subTT
	, subTT_cutM
	, subTT_noLoops
	, subTTK_noLoops
	, subTT_all
	, SubM
	, subVV
	, subCidCid)
where
import Type.Plate.Collect
import Type.Pretty		()
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Transform
import DDC.Type.Kind
import DDC.Var
import Data.Traversable		(mapM)
import Util			hiding (mapM)
import Prelude			hiding (mapM)
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Type.Util.Substitute"

type SubM	= State [(Type, Type)]


-- | Do a loop cutting substitution on this type.
subTT 	:: Map Type Type 	-- ^ types to substitute for.
	-> Type 		-- ^ type to substitute into.
	-> (Type, [(Type, Type)])	-- ^ result type and list of types from
					--	the substitution that were looping

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

-- | Substitute type for types in some kind.
--	Cut loop cutting substition on this type.
--	panic if there are any loops through the data portion.
subTTK_noLoops
	:: Map Type Type
	-> Kind
	-> Kind

subTTK_noLoops sub kk
 = let	(kk', loops)	= runState (subTTK_cutM sub Set.empty kk) []
   in	if null loops
   		then kk'
		else panic stage
			$ "subTTK_noLoops: found loops through data portion of type\n"
			% "  kk            = " % kk		% "\n\n"
			% "  loops through = " % loops		% "\n"

	
-- | Do a loop cutting substitution on this type.
--	If we find any loops through the data potion of this type then
--	push them as errors into the monad.
--
subTT_cutM 
	:: Map Type Type	-- types to substitute for
	-> Set Type		-- types that have already been substituted, and should be cut
	-> Type 		-- type to substitute for
	-> SubM Type
	
subTT_cutM sub cut tt
 = do	sub'	<- cutSub sub
 	subTT_cutM' sub' cut tt
	
subTT_cutM' sub cut tt
 = let 	down	= subTT_cutM' sub cut
	downK	= subTTK_cutM sub cut
   in case tt of
 	TForall v k t		
	 -> do	t'	<- down t
		k'	<- downK k
	 	return	$ TForall v k' t'

	TFetters t fs
	 -> do	t'	<- down t
		fs'	<- mapM (subTTf_cutM' sub cut) fs
	 	return	$ TFetters t' fs'

	TConstrain t crs@(Constraints crsEq crsMore crsOther)
	 -> do	t'		<- down t
		crsEq'		<- mapM  (subTT_cutM' sub cut) crsEq
		crsMore'	<- mapM  (subTT_cutM' sub cut) crsMore
		crsOther'	<- mapM  (subTTf_cutM' sub cut) crsOther
		return	$ TConstrain t (Constraints crsEq' crsMore' crsOther')

	TSum k ts
	 -> do	ts'	<- mapM down ts
	 	return	$ TSum k ts'
		
	TApp t1 t2
	 -> do	t1'	<- down t1
	 	t2'	<- down t2
		return	$ TApp t1' t2'
		
	TCon{}		-> return tt
				
	TError{}	-> return tt

	TVar{} 		-> subTT_enter sub cut tt

	_ -> panic stage $ "subTT_cutM': no match for " % tt

subTTK_cutM sub cut kk
 = let	downT	= subTT_cutM sub cut
	downK	= subTTK_cutM sub cut

   in case kk of
	KNil	-> return kk

	KCon{}	-> return kk

	KFun k1 k2
	 -> do	k1'	<- downK k1
		k2'	<- downK k2
		return	$ KFun k1' k2'

	KApp k1 t2
	 -> do	k1'	<- downK k1
		t2'	<- downT t2
		return	$ KApp k1' t2'
			
	KSum ks
	 -> do	ks'	<- mapM downK ks
		return	$ KSum ks'
		
	
subTT_enter sub cut tt

	-- see if we can substitute something
	| Just tt'		<- Map.lookup tt sub
	= let res
		| Set.member tt cut
		, k	<- kindOfType tt
		= let res2 :: SubM Type
		      res2
			-- Loops in effect and closure types can be cut by replacing
			--	the looping variable with bottom
			| k == kEffect	= return tPure
			| k == kClosure	= return tEmpty

			-- Loops in other parts of the graph are errors.
			| otherwise	
			= do	modify $ \s -> (tt, tt') : s
				return	tt
		  in res2
		
		| otherwise
		= subTT_cutM' sub (Set.insert tt cut) tt'
	  in	res

	-- nothing can be substituted
	| otherwise
	= return tt
	
subTTf_cutM' sub cut ff
 = case ff of
 	FConstraint v ts
	 -> do	ts'	<- mapM (subTT_cutM' sub cut) ts
	 	return	$ FConstraint v ts'
		
	FWhere t1 t2
	 -> do	t2'	<- subTT_cutM' sub cut t2
	 	return	$ FWhere t1 t2'
		
	FMore t1 t2
	 -> do	t2'	<- subTT_cutM' sub cut t2
	 	return	$ FMore t1 t2'
		
	FProj j v t1 t2
	 -> do	t1'	<- subTT_cutM' sub cut t1
	 	t2'	<- subTT_cutM' sub cut t2
		return	$ FProj j v t1' t2'



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
	| isValueType t1
	= if Set.member t1 $ collectTClassVars t2
		then do	modify $ \s -> (t1, t2) : s
			return $ Nothing

		else return $ Just (t1, t2)
		
	| otherwise
	= return $ Just (t1, t2)	

-- | Substiute vars for vars in this type
subVV :: Map Var Var -> Type -> Type
subVV sub 
 = transformV 
 	(\v -> case Map.lookup v sub of
 			Just v'	-> v'
			Nothing	-> v)

-- | Substitute cids for cids in this type
subCidCid sub
 = transformCid 
 	(\cid -> case Map.lookup cid sub of
			Just cid' -> cid'
			Nothing   -> cid)
			

