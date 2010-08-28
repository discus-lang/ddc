{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Construction and destruction of common compound things.
--	Also known as 'smart' constructors and destructors.
module DDC.Type.Compounds
	(-- * Superkinds
	  makeSuperFun
		
	 -- * Kinds
	, makeKFuns
	, takeKApps
	, makeKApps
	, makeKSum
	, makeDataKind
	, resultKind

	  -- * Binds
	, takeVarOfBind
	, takeVarOfBound

	  -- * Varish things
	, takeTClass

	  -- * Application
	, makeTApp
	, takeTApps
	
	  -- * Functions
	, makeTFun
	, makeTFunsPureEmpty
	, makeTFunsEC
	, takeTFun
	, flattenTFuns
	
	  -- * Data
	, makeTData
	, takeTData
	
	  -- * Sums
  	, makeTSum
	, flattenTSum
	
	  -- * Witnesses
	, makeTWitness
	, takeTWitness	
	
	  -- * Closures
 	, makeTFree
	, makeTFreeBot
	, takeTFree
	, makeTDanger
	, takeTDanger
	, dropTFreesIn
	
	  -- * Fetters
	, takeBindingVarF
	, constraintsOfFetters
	, fettersOfConstraints
	, addFetterToConstraints
	
	  -- * Constraints
	, makeTConstrain
	, addConstraints
	, addConstraintsEq
	, addConstraintsEqVT
	, addConstraintsMore
	, addConstraintsOther
	, pushConstraints
	, pushConstraintsEq
	, pushConstraintsMore
	, pushConstraintsOther
	
	 -- * Quantification
	, makeTForall_front
	, makeTForall_back
	, takeTForall
	, addContextUnderForalls
	, addContextsUnderForalls
	)
where
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Kind
import DDC.Var
import Data.List
import Util
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "DDC.Type.Compounds"

-- Superkinds -------------------------------------------------------------------------------------
-- | Create a superkind function.
makeSuperFun :: [Kind] -> Super -> Super
makeSuperFun [] 	s	= s
makeSuperFun (k:ks) 	s	= SFun k (makeSuperFun ks s)


-- Kinds ------------------------------------------------------------------------------------------
-- | Make a kind function.
makeKFuns :: [Kind] -> Kind -> Kind
makeKFuns [] kk	= kk
makeKFuns (k:ks) kk	= KFun k (makeKFuns ks kk)


-- | Make a dependent kind application from a list of types.
makeKApps :: Kind -> [Type] -> Kind
makeKApps k []	= k
makeKApps k ts	= makeKApps' k $ reverse ts

makeKApps' k tt
 = case tt of
	[]	-> panic stage "makeKApps: this never happens :P"
	t : []	-> KApp k t
	t : ts	-> KApp (makeKApps' k ts) t 


-- | Flatten out a dependent kind application into its parts.
takeKApps :: Kind -> Maybe (Kind, [Type])
takeKApps kk
 = case kk of
	KCon{} -> Just (kk, [])

	KApp k1 t2
	  -> let Just (k1', ts)	= takeKApps k1
	     in	 Just (k1', ts ++ [t2])
	
	_ -> Nothing


-- | Join some kind classes
makeKSum :: [Kind] -> Kind
makeKSum ts
 = case nub ts of
 	[t]	-> t
	ts'	-> KSum ts'


-- Make a kind from the parameters to a data type
makeDataKind :: [Var] -> Kind
makeDataKind vs
 	= foldl (flip KFun) kValue 
	$ map (\v -> let Just k = kindOfSpace (varNameSpace v) in k) 
	$ reverse vs


-- | Get the result of applying all the paramters to a kind.
resultKind :: Kind -> Kind
resultKind kk
 = case kk of
 	KFun _ k2	-> resultKind k2
	_		-> kk


-- Binds ------------------------------------------------------------------------------------------
-- | Get the `Var` from a `Bind`, if any.
takeVarOfBind :: Bind -> Maybe Var
takeVarOfBind bb
 = case bb of
	BNil		-> Nothing
 	BVar v		-> Just v
	BMore v _	-> Just v


-- | Get the `Var` from a `Bound`, if any.
takeVarOfBound :: Bound -> Maybe Var
takeVarOfBound bb
 = case bb of
	UVar v		-> Just v
 	UMore v _	-> Just v
	_		-> Nothing


-- Varish things ----------------------------------------------------------------------------------
-- | Get the `ClassId` from a `TClass`, if any.
takeTClass :: Type -> Maybe ClassId
takeTClass (TVar _ (UClass cid))	= Just cid
takeTClass _				= Nothing


-- General Type Application -----------------------------------------------------------------------
-- | Make a type application. 
makeTApp :: Type -> [Type] -> Type
makeTApp t []	= t
makeTApp t ts 	= makeTApp' $ reverse (t : ts)

makeTApp' xx
 = case xx of
	[]		-> panic stage $ "makeTApp': this never happens :P"
 	x  : []		-> x
	x1 : xs		-> TApp (makeTApp' xs) x1
	
	
-- | Flatten a type application into its parts
takeTApps :: Type -> [Type]
takeTApps tt
 = case tt of
	TApp t1 t2	-> t1 : takeTApps t2
	_		-> [tt]


-- Function Types ---------------------------------------------------------------------------------
-- | Make a single function type.
makeTFun :: Type -> Type -> Effect -> Closure -> Type
makeTFun t1 t2 eff clo
	= TApp (TApp (TApp (TApp (TCon TyConFun) t1) t2) eff) clo


-- | Convert a list of types:	@[t1, t2, t3, t4]@
--   into a function type:      @t1 -> (t2 -> (t3 -> t4))@,
--   using the given effect and closure to annotate every function constructor.
--
--  The list of types must be non-empty, else `panic`.
--
makeTFunsEC :: Effect -> Closure -> [Type] -> Type
makeTFunsEC _   _   (x:[])	= x
makeTFunsEC eff clo (x:xs)	= makeTFun x (makeTFunsEC eff clo xs) eff clo
makeTFunsEC _   _   []		= panic stage $ "makeTFunEC: empty list"


-- | Like `makeTFunsEC` but each function constructor is given a pure effect
--   and an empty closure.
makeTFunsPureEmpty :: [Type] -> Type
makeTFunsPureEmpty []		= panic stage $ "makeTFunsPureEmpty: empty list"
makeTFunsPureEmpty xx		= makeTFunsEC tPure tEmpty xx


-- | Take the arguments of a function type.
takeTFun :: Type -> Maybe (Type, Type, Effect, Closure)
takeTFun tt
 	| TApp (TApp (TApp (TApp fun t1) t2) eff) clo	<- tt
	, TCon TyConFun{}	<- fun
	= Just (t1, t2, eff, clo)
	
	| otherwise
	= Nothing


-- | Flatten a function type into its arguments and return type.
--   The last element of the list is the return type.	
flattenTFuns :: Type -> [Type]
flattenTFuns xx
 = case takeTFun xx of
	Just (t1, t2, _, _)	-> t1 : flattenTFuns t2
	_			-> [xx]


-- Data Types -------------------------------------------------------------------------------------
-- | Make a data type given the var and kind of its constructor, and its arguments.
makeTData :: Var -> Kind -> [Type] -> Type
makeTData v k ts
 	= makeTApp (TCon TyConData { tyConName = v, tyConDataKind = k }) ts

	
-- | Take a data type from an application.
takeTData :: Type -> Maybe (Var, Kind, [Type])
takeTData tt
 = case tt of
 	TCon TyConData { tyConName = v, tyConDataKind = k }
		-> Just (v, k, [])
		
	TApp t1 t2
	 -> case takeTData t1 of
	 	Just (v, k, ts)	-> Just (v, k, ts ++ [t2])
		Nothing		-> Nothing
		
	_ -> Nothing


-- Sums -------------------------------------------------------------------------------------------
-- | Make a new sum from a list of types.
--	If the list contains further sums then the elements are unpacked into the result.
--	If there is only one element in the list then return that element instead of making a sum.
makeTSum :: Kind -> [Type] -> Type
makeTSum k ts
 = case nub $ catMap flattenTSum ts of
	[t']	-> t'
	ts'	-> TSum k ts'


-- | Flatten a sum into its components. If the sum contains further nested
--   sums then these are also flattened.
flattenTSum :: Type -> [Type]
flattenTSum tt
 = case tt of
	TSum _ ts		-> catMap flattenTSum ts
	_			-> [tt]


-- Witnesses --------------------------------------------------------------------------------------
-- | Make a witness application.
makeTWitness :: TyConWitness -> Kind -> [Type] -> Type
makeTWitness con k ts
	= makeTApp (TCon (TyConWitness con k)) ts


-- | Take a witness from its constructor application.
--	Returns the witness constructor and its kind, along with the type args.
takeTWitness :: Type -> Maybe (TyConWitness, Kind, [Type])
takeTWitness tt
	| TCon (TyConWitness v k) <- tt
	= Just (v, k, [])

	| TApp t1 t2		<- tt
	, Just (v, k, ts)	<- takeTWitness t1
	= Just (v, k, ts ++ [t2])
	
	| otherwise
	= Nothing


-- Closures ---------------------------------------------------------------------------------------
-- | Take an application of the $Free closure constructor.
takeTFree :: Type -> Maybe (Var, Type)
takeTFree tt
 = case tt of
	TApp (TCon (TyConClosure (TyConClosureFreeType v) _)) t2
	   -> Just (v, t2)

	TApp (TCon (TyConClosure (TyConClosureFreeRegion v) _)) t2
	   -> Just (v, t2)

	TApp (TCon (TyConClosure (TyConClosureFree v) _)) t2
	   -> Just (v, t2)
	
	_  -> Nothing


-- | Make an application of one of the $Free closure constructors.
--	The constructor depends on the kind of the type being used,
--	but it's usually straight-forward to determine.
--
--  NOTE: we can't just call `kindOfType` here because the parser calls this
--        before kind inference, and we won't know the real kinds of all 
--        the type variabes yet.
--
makeTFree :: Var -> Type -> Maybe Closure
makeTFree v tt
 = case tt of
	TVar k _ 	-> makeTFreeWithKind k v tt
	TSum k _	-> makeTFreeWithKind k v tt
	
	-- Closure constructors always return closures
	TApp (TCon TyConClosure{}) _	-> makeTFreeWithKind kClosure v tt
	
	TApp{}		-> makeTFreeWithKind kValue v tt
	TForall{}	-> makeTFreeWithKind kValue v tt
	TConstrain{}	-> makeTFreeWithKind kValue v tt
	TCon{}		-> makeTFreeWithKind kValue v tt
	_		-> Nothing
	
	
makeTFreeWithKind :: Kind -> Var -> Type -> Maybe Closure
makeTFreeWithKind k v t
	| isClosureKind k	= Just $ TApp (tFree       v) t
	| isValueKind   k	= Just $ TApp (tFreeType   v) t
	| isRegionKind  k	= Just $ TApp (tFreeRegion v) t
	| otherwise		= Nothing


-- | Make a TFree if the type has an appropriate kind, otherwise make a Bot
makeTFreeBot :: Var -> Type -> Closure
makeTFreeBot v t
	| isClosure t		= TApp (tFree       v) t
	| isValueType t		= TApp (tFreeType   v) t
	| isRegion  t 		= TApp (tFreeRegion v) t
	| otherwise		= tBot kClosure


-- | Make an application of the $Danger closure constructor.
makeTDanger :: Type -> Type -> Type
makeTDanger t1 t2
 	= TApp (TApp tDanger t1) t2


-- | Take an application of the $Danger closure constructor.
takeTDanger :: Type -> Maybe (Type, Type)
takeTDanger tt 
 = case tt of
	TApp (TApp (TCon (TyConClosure TyConClosureDanger _)) t2) t3
	 -> Just (t2, t3)
	
	_   -> Nothing


-- | Drop TFree terms concerning value variables in this set
dropTFreesIn :: Set Var -> Closure -> Closure
dropTFreesIn vs clo
 	= makeTSum kClosure
	$ filter (\c -> case takeTFree c of
			 Just (v, _)	-> not $ Set.member v vs
			 Nothing	-> True)
	$ flattenTSum clo


-- Fetters -----------------------------------------------------------------------------------------	
-- | Take the binding var from a `FWhere`, if any.
takeBindingVarF :: Fetter -> Maybe Var
takeBindingVarF ff
 = case ff of
 	FWhere (TVar _ (UVar  v))   _	-> Just v
 	FWhere (TVar _ (UMore v _)) _	-> Just v
	_				-> Nothing


-- | Convert a list of fetters to a constraints
constraintsOfFetters :: [Fetter] -> Constraints
constraintsOfFetters fs
	= foldr addFetterToConstraints 
		(Constraints Map.empty Map.empty [])
		fs

-- | Convert a set of constraints to a list of fetters
fettersOfConstraints :: Constraints -> [Fetter]
fettersOfConstraints crs
 	=  [FWhere t1 t2	| (t1, t2) <- Map.toList $ crsEq   crs]
	++ [FMore  t1 t2	| (t1, t2) <- Map.toList $ crsMore crs]
	++ crsOther crs
	


-- | Add a fetter to a some constraints.
addFetterToConstraints :: Fetter -> Constraints -> Constraints
addFetterToConstraints ff crs
 = case ff of
	FWhere t1 t2
	 -> crs { crsEq	  = Map.insert t1 t2 (crsEq crs) }
	
	FMore t1 t2
	 -> crs { crsMore = Map.insert t1 t2 (crsMore crs) }
	
	FConstraint{}
	 -> crs { crsOther = ff : crsOther crs }
	
	FProj{} 
	 -> crs { crsOther = ff : crsOther crs }


-- Constraints ------------------------------------------------------------------------------------
-- | If the given constraints are non empty then add a TConstrain to the type, 
--   otherwise return the original type.
makeTConstrain :: Type -> Constraints -> Type
makeTConstrain tt crs
 = if not (Map.null $ crsEq crs) || not (Map.null $ crsMore crs) || not (null $ crsOther crs)
    	then TConstrain tt crs
	else tt


-- | Add some constraints to a type
addConstraints :: Constraints -> Type -> Type
addConstraints crs@(Constraints crsEq1 crsMore1 crsOther1) tt
 = case tt of
	TConstrain t (Constraints crsEq2 crsMore2 crsOther2)
	 -> addConstraints' 
		(Constraints 
			(Map.union crsEq1   crsEq2)
			(Map.union crsMore1 crsMore2)
			(crsOther1 ++ crsOther2))
		t
			
	_ -> addConstraints' crs tt
	
addConstraints' crs@(Constraints crsEq crsMore crsOther) tt
 	| Map.null crsEq
	, Map.null crsMore
	, null crsOther
	= tt
	
	| otherwise
	= TConstrain tt crs


-- | Add some eq constaints to a type
addConstraintsEq :: Map Type Type -> Type -> Type
addConstraintsEq crs tt
	= addConstraints (Constraints crs Map.empty []) tt


-- | Add some eq constaints to a type.
---- GAH: Calling kindOfType here is really slow. Bad idea to call this fn.
addConstraintsEqVT :: Map Var Type -> Type -> Type
addConstraintsEqVT crs tt
 = let 	crs'	= Map.fromList
		$ [(TVar (kindOfType t) (UVar v), t) 
			| (v, t) <- Map.toList crs]

   in	addConstraintsEq crs' tt


-- | Add some more-than constaints to a type
addConstraintsMore :: Map Type Type -> Type -> Type
addConstraintsMore crs tt
	= addConstraints (Constraints Map.empty crs []) tt
	
	
-- | Add some other constaints to a type
addConstraintsOther :: [Fetter] -> Type -> Type
addConstraintsOther crs tt
	= addConstraints (Constraints Map.empty Map.empty crs) tt


-- | Add come constraints to a type, pushing them under any enclosing foralls.
pushConstraints :: Constraints -> Type -> Type
pushConstraints crs tt
 = case tt of
	TForall v k t	-> TForall v k (pushConstraints crs t)
	_		-> addConstraints crs tt


-- | Push some eq constaints into a type.
pushConstraintsEq :: Map Type Type -> Type -> Type
pushConstraintsEq crs tt
	= pushConstraints (Constraints crs Map.empty []) tt


-- | Add some more-than constaints to a type
pushConstraintsMore :: Map Type Type -> Type -> Type
pushConstraintsMore crs tt
	= pushConstraints (Constraints Map.empty crs []) tt
	
	
-- | Add some other constaints to a type
pushConstraintsOther :: [Fetter] -> Type -> Type
pushConstraintsOther crs tt
	= pushConstraints (Constraints Map.empty Map.empty crs) tt


-- Quantification ---------------------------------------------------------------------------------
-- | Add some forall bindings to the front of this type.
makeTForall_front :: [(Bind, Kind)] -> Type -> Type
makeTForall_front vks tt
 = makeTForall_front' (reverse vks) tt
 
makeTForall_front' bks tt
 = case bks of
	[]		-> tt
	(b, k) : rest	-> makeTForall_front' rest (TForall b k tt)


-- | Add some forall bindings to this type, placing them after others already there.
makeTForall_back :: [(Bind, Kind)] -> Type -> Type
makeTForall_back [] tt			= tt
makeTForall_back bks@((b, k) : bksRest) tt
 = case tt of
	TForall b' k' t
	  -> TForall b' k' (makeTForall_back bks t)
	
	_ -> TForall b k (makeTForall_back bksRest tt)


-- | Slurp outer forall bindings that bind vars from this type,
--   not including contexts that have their `Bind` set to `BNil`.
takeTForall :: Type -> ([(Bind, Kind)], Type)
takeTForall tt
 = case tt of
 	TForall b@BVar{} k t	
	 -> let	(bksRest, tRest)	= takeTForall t
	    in	( (b, k) : bksRest, tRest)

 	TForall b@BMore{} k t	
	 -> let	(bksRest, tRest)	= takeTForall t
	    in	( (b, k) : bksRest, tRest)
	    
	_ -> ([], tt)


-- | Add a new type class context to a type,
--	pushing it under any enclosing foralls.
addContextUnderForalls :: Kind -> Type -> Type
addContextUnderForalls k tt
 = case tt of
 	TForall b@BVar{}  k' t	-> TForall b k' (addContextUnderForalls k t)
 	TForall b@BMore{} k' t	-> TForall b k' (addContextUnderForalls k t)
	_			-> TForall BNil k tt

-- | Add some new type class contexts to a type,
--	pushing them under any enclosing foralls.
addContextsUnderForalls :: [Kind] -> Type -> Type
addContextsUnderForalls []	   t	= t
addContextsUnderForalls (k:ks) t	
	= TForall BNil k (addContextsUnderForalls ks t)
