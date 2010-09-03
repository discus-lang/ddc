{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Type substitution.
module DDC.Type.Operators.Substitute
	( subTT_noLoops
	, subTTK_noLoops
	, subTT_everywhere
	, subVT_everywhere
	, subVV_everywhere
	, subCidCid_everywhere)
where
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Transform
import DDC.Type.Kind
import DDC.Var
import DDC.Type.Collect.FreeTVars
import DDC.Type.Pretty		()
import Data.Traversable		(mapM)
import Util			hiding (mapM)
import Prelude			hiding (mapM)
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Type.Util.Substitute"


{- HISTORY: We used to used this for handling infinite type errors, and may change back to it later.
-- | Do a loop cutting substitution on this type.
--	For loops through effect and closure variables we replace the occurrence with bottom.
--	If there are loops through other variables we return the list of looping constraints
--	in the second element of the tuple.
subTT_cutLoops
	:: Map Type Type	 	-- ^ Types to substitute for.
	-> Type 			-- ^ Type  to substitute into.
	-> (Type, [(Type, Type)])	-- ^ Result type and list of constraints that were looping, if any.

subTT_cutLoops sub tt	
	= runState (subTT_cutM sub Set.empty tt) []
-}

-- | State monad to collect up the loops we've found.
type SubM	= State [(Type, Type)]


-- | Do a loop cutting substitution on this type.
--	If we find any loops through the data potion of this type then record the looping
--	constraint in the state monad.
subTT_cutM 
	:: Map Type Type		-- ^ Types to substitute for.
	-> Set Type			-- ^ Types lhs that have already been substituted.
	-> Type 			-- ^ Type to substitute into.
	-> SubM Type
	
subTT_cutM sub cut tt
 = do	sub'	<- cutSub sub
 	subTT_cutM' sub' cut tt
	
subTT_cutM' sub cut tt
 = let 	down	= subTT_cutM' sub cut
	downK	= subTTK_cutM sub cut
   in case tt of
	TConstrain t (Constraints crsEq crsMore crsOther)
	 -> do	t'		<- down t
		crsEq'		<- mapM  (subTT_cutM' sub cut) crsEq
		crsMore'	<- mapM  (subTT_cutM' sub cut) crsMore
		crsOther'	<- mapM  (subTT_fetter_cutM' sub cut) crsOther
		return	$ TConstrain t' (Constraints crsEq' crsMore' crsOther')

 	TForall v k t	-> liftM3 TForall (return v) (downK k) (down t) 
	TSum k ts	-> liftM2 TSum    (return k) (mapM down ts)
	TApp t1 t2	-> liftM2 TApp    (down t1)  (down t2)
	TCon{}		-> return tt
	TError{}	-> return tt
	TVar{} 		-> subTT_enter sub cut tt
	_ 		-> panic stage $ "subTT_cutM': no match for " % tt


subTTK_cutM sub cut kk
 = let	downT	= subTT_cutM sub cut
	downK	= subTTK_cutM sub cut
   in case kk of
	KNil		-> return kk
	KCon{}		-> return kk
	KFun k1 k2	-> liftM2 KFun (downK k1) (downK k2)
	KApp k1 t2	-> liftM2 KApp (downK k1) (downT t2)
	KSum ks		-> liftM  KSum (mapM downK ks)
		
	
subTT_enter sub cut tt

	-- see if we can substitute something
	| Just tt'		<- Map.lookup tt sub
	= let res
		| Set.member tt cut
		, k		<- kindOfType tt
		= let res2 :: SubM Type
		      res2
			-- Loops in effect and closure types are cut by replacing the
			-- looping variable with bottom.
			| k == kEffect	= return tPure
			| k == kClosure	= return tEmpty

			-- Loops in other parts of the graph are recorded in the state.
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
	
subTT_fetter_cutM' sub cut ff
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


-- | Check substitution for loops through the data portion of the type
cutSub :: Map Type Type -> SubM (Map Type Type)
cutSub sub
 = do 	sub'	<- liftM Map.fromList
		$  liftM catMaybes
		$  mapM cutF
		$  Map.toList sub
		
	return	sub'


cutF :: (Type, Type) -> SubM (Maybe (Type, Type))
cutF (t1, t2)
	-- If the binding var is in the rhs then we've got an infinite type error
	| isValueType t1
	= if Set.member t1 $ freeTClassVars t2
		then do	modify $ \s -> (t1, t2) : s
			return $ Nothing

		else return $ Just (t1, t2)
		
	| otherwise
	= return $ Just (t1, t2)	



-- No Loops Versions ------------------------------------------------------------------------------
-- | Do a loop cutting substition of types for types in some type,
--	but panic if there are any loops through the value type portion.
--	For loops through effect and closure variables we replace the occurrence with bottom.

subTT_noLoops :: Map Type Type -> Type -> Type
subTT_noLoops sub tt
	| (tt', loops)	<- runState (subTT_cutM sub Set.empty tt) []
	= if null loops
   		then tt'
		else panic stage
			$ "subTT_noLoops: found loops through data portion of type\n"
			% "  tt            = " % tt		% "\n\n"
			% "  loops through = " % loops		% "\n"


-- | Do a loop cutting substitution of types for types in some kind, 
--	but panic if there are loops through the value type portion.
--	For loops through effect and closure variables we replace the occurrence with bottom.
subTTK_noLoops :: Map Type Type -> Kind -> Kind
subTTK_noLoops sub kk
 	| (kk', loops)	<- runState (subTTK_cutM sub Set.empty kk) []
   	= if null loops
   		then kk'
		else panic stage
			$ "subTTK_noLoops: found loops through data portion of type\n"
			% "  kk            = " % kk		% "\n\n"
			% "  loops through = " % loops		% "\n"


-- Transform based substitutions ------------------------------------------------------------------
-- | Do a shallow substition of type for types everywhere in some thing.
subTT_everywhere
	:: TransM (State ()) a
	=> Map Type Type
	-> a -> a

subTT_everywhere sub tt
 	= transformT (\t -> case t of
			TVar{}
			 -> case Map.lookup t sub of
				Just t'	-> t'
				Nothing	-> t
									
			_	-> t)
	$ tt


-- | Do a shallow substition of type for variables everywhere in some thing.
subVT_everywhere
	:: TransM (State ()) a
	=> Map Var Type
	-> a -> a

subVT_everywhere sub tt
 	= transformT (\t -> case t of
			TVar _ (UVar v)
			 -> case Map.lookup v sub of
				Just t'	-> t'
				Nothing	-> t

			TVar _ (UMore v _)
			 -> case Map.lookup v sub of
				Just t'	-> t'
				Nothing	-> t
									
			_	-> t)
	$ tt


-- | Substiute vars for vars everywhere in this type.
subVV_everywhere :: Map Var Var -> Type -> Type
subVV_everywhere sub 
 = transformV 
 	(\v -> case Map.lookup v sub of
 			Just v'	-> v'
			Nothing	-> v)


-- | Substitute cids for cids in this type.
subCidCid_everywhere 
	:: TransM (State ()) a
	=> Map ClassId ClassId 
	-> a -> a

subCidCid_everywhere sub
 = transformCid 
 	(\cid -> case Map.lookup cid sub of
			Just cid' -> cid'
			Nothing   -> cid)


