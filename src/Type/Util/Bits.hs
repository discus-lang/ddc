
-- | Bits and pieces for working with types.
module Type.Util.Bits
	-- simple
	( isTBot
	, isTApp
	, isSomeTVar
	, isTClass
	, isFConstraint
	, isFWhere
	, isFMore
	
	, isUnboxedT
	, makeTFunEC

	-- arity
	, takeValueArityOfType
	
	-- projections
	, takeVarOfBind
	, takeBindingVarF
	, takeCidOfTClass
	
	-- crushing
	, crushT

	-- sums
	, makeTSum
	, flattenTSum

	-- type application
	, makeTApp
	, takeTApps

	, makeTData
	, takeTData
	, makeTFun
	, takeTFun
	, makeTFuns_pureEmpty
	, flattenFun
	, makeTFree
	, takeTFree
	, makeTDanger
	, takeTDanger
	
	-- closure
	, dropTFreesIn

	-- forall
	, makeTForall_front
	, makeTForall_back
	, slurpVarTForall
	, makeKFuns

	-- witnesses
	, makeTWitness
	, takeTWitness

	-- fetters
	, makeTFetters
	, takeTFetters
	, addFetters
	, addFetters_front
	
	-- constraints
	, toFetterFormT
	, toConstrainFormT
	, addConstraints
	
	-- contexts
	, addContext
	, addContexts
	
	-- supertypes
	, unflattenSuper
	
	-- stripping
	, stripToBodyT
	)
where
import Util
import Type.Exp
import Type.Plate.Trans
import Type.Builtin
import DDC.Main.Error
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set

-----
stage	= "Type.Util.Bits"


-- Simple things -----------------------------------------------------------------------------------
isTBot tt
 = case tt of
	TSum _ []	-> True
	_		-> False

isTApp tt
 = case tt of
 	TApp{}		-> True
	_		-> False

isSomeTVar tt
 = case tt of
 	TVar{}		-> True
	TVarMore{}	-> True
	_		-> False

isTClass tt
 = case tt of
 	TClass{}	-> True
	_		-> False

isFConstraint ff
 = case ff of
 	FConstraint v ts -> True
	_		 -> False

isFWhere ff
 = case ff of
 	FWhere{}	-> True
	_ 		-> False

isFMore ff
 = case ff of
 	FMore{}		-> True
	_ 		-> False

-- | Check if a type represents some unboxed value
isUnboxedT :: Type -> Bool
isUnboxedT t
 = case takeTData t of
 	Just (v, _, _)
	 | last (varName v) == '#'	-> True	 
	_				-> False

-- | makeTFunEC
--	Converts a list of types:	[t1, t2, t3, t4]
--	into a function type:		t1 -> (t2 -> (t3 -> t4))
--	Uses the same effect and closure on every constructor
--
makeTFunEC ::	Effect -> Closure -> [Type]	-> Type
makeTFunEC	eff clo (x:[])			= x
makeTFunEC	eff clo (x:xs)			= makeTFun x (makeTFunEC eff clo xs) eff clo
makeTFunEC	_   _   []			= panic stage $ "makeTFunEC: not enough args for function"


-- Arity ------------------------------------------------------------------------------------------

-- | Take the arity of a type, ie how many arguments we can apply to it.
--	If the type is not a value type this returns Nothing
takeValueArityOfType :: Type -> Maybe Int
takeValueArityOfType tt
 = case tt of
	TNil		-> Nothing
	TForall	b k t	-> takeValueArityOfType t
	TFetters t fs	-> takeValueArityOfType t
	TConstrain t cs	-> takeValueArityOfType t

	TApp{}		
	 | Just (t1, t2, eff, clo)	<- takeTFun tt
	 , Just a2			<- takeValueArityOfType t2
	 -> Just $ 1 + a2
	
	 | Just _			<- takeTData tt
	 -> Just 0
	
	TSum{}		-> Nothing
	TCon{}		-> Just 0
	TVar{}		-> Just 0
	TIndex{}	-> Nothing
	TClass{}	-> Just 0
	TError{}	-> Nothing
	TVarMore{}	-> Just 0
	

-- Projections -------------------------------------------------------------------------------------
-- | Get the var from a forall binder
takeVarOfBind :: Bind -> (Maybe Var)
takeVarOfBind bb
 = case bb of
	BNil		-> Nothing
 	BVar v		-> Just v
	BMore v t	-> Just v


takeCidOfTClass :: Type -> Maybe ClassId
takeCidOfTClass (TClass k cid)	= Just cid
takeCidOfTClass _		= Nothing


-- | Take the binding var from FLet's 
takeBindingVarF :: Fetter -> Maybe Var
takeBindingVarF ff
 = case ff of
 	FWhere (TVar k v) t2	-> Just v
	_			-> Nothing


-- Crushing ----------------------------------------------------------------------------------------
-- | do some simple packing
crushT :: Type -> Type
crushT t
 = let	t'	= transformT crushT1 t
   in	if t == t'
   	 then	t
	 else 	crushT t'

crushT1 :: Type -> Type
crushT1 tt
 = case tt of
 	TSum k ts			
	 -> makeTSum k 		$ flattenTSum tt

	TApp{}
	 | Just (v,  t2)	<- takeTFree tt
	 , Just (v', t)		<- takeTFree t2
	 -> makeTFree v t
	
	 | Just (v, t2)		<- takeTFree tt
	 , TSum k ts		<- t2
	 -> makeTSum k $ map (makeTFree v) ts
	
	_	-> tt


-- Sums --------------------------------------------------------------------------------------------
-- | Make a new sum from a list of type, crushing the list and substituting
--	TPure\/TEmpty if there is nothing to sum. If there only one thing
--	after crushing then return that thing instead of a sum.
makeTSum :: Kind -> [Type] -> Type
makeTSum k ts
 = case nub $ catMap flattenTSum ts of
	[t']	-> t'
	ts'	-> TSum k ts'

-- | Crush nested TSums into their components.
flattenTSum :: Type -> [Type]
flattenTSum tt
 = case tt of
	TSum k ts		-> catMap flattenTSum ts

	TApp{}
	 | Just (v, TSum k [])	<- takeTFree tt
	 -> []
	
	_			-> [tt]


-- Type Application --------------------------------------------------------------------------------
-- | Make a type application
makeTApp :: [Type] -> Type
makeTApp ts = makeTApp' $ reverse ts

makeTApp' xx
 = case xx of
	[]		-> panic stage $ "makeTApp': empty list"
 	x : []		-> x
	x1 : xs		-> TApp (makeTApp' xs) x1
	
-- | Flatten a type application into its parts
takeTApps :: Type -> [Type]
takeTApps tt
 = case tt of
	TApp t1 t2	-> t1 : takeTApps t2
	_		-> [tt]


-- | Make a data type
makeTData :: Var -> Kind -> [Type] -> Type
makeTData v k ts
 = makeTApp (TCon TyConData { tyConName = v, tyConDataKind = k } : ts )

	
-- | Take a data type from a data type constructor application.
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


-- | Make a single function type
makeTFun :: Type -> Type -> Effect -> Closure -> Type
makeTFun t1 t2 eff clo
	= TApp (TApp (TApp (TApp (TCon TyConFun) t1) t2) eff) clo

-- | Make a chained function type with pure effects and empty closures
makeTFuns_pureEmpty :: [Type] -> Type
makeTFuns_pureEmpty xx
 = case xx of
 	x : []		-> x
	x : xs		-> makeTFun x (makeTFuns_pureEmpty xs) tPure tEmpty

-- | Take a function type from a type constructor application.
takeTFun :: Type -> Maybe (Type, Type, Effect, Closure)
takeTFun tt
 	| TApp (TApp (TApp (TApp fun t1) t2) eff) clo	<- tt
	, TCon TyConFun{}	<- fun
	= Just (t1, t2, eff, clo)
	
	| otherwise
	= Nothing

-- | Flatten a function type into parts
flattenFun :: Type -> [Type]
flattenFun xx
 = case takeTFun xx of
	Just (t1, t2, _, _)	-> t1 : flattenFun t2
	_			-> [xx]


-- | Take an application of the $Free closure constructor.
takeTFree :: Type -> Maybe (Var, Type)
takeTFree tt
 = case tt of
	TApp (TCon (TyConClosure (TyConClosureFree v) kind)) t2
	   -> Just (v, t2)
	
	_  -> Nothing

-- | Make an application of the $Free closure constructor.
makeTFree :: Var -> Type -> Type
makeTFree var tt
	= TApp (tFree var) tt


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

-- Closure -----------------------------------------------------------------------------------------

-- | Drop TFree terms concerning value variables in this set
dropTFreesIn :: Set Var -> Closure -> Closure
dropTFreesIn vs clo
 	= makeTSum kClosure
	$ filter (\c -> case takeTFree c of
			 Just (v, _)	-> not $ Set.member v vs
			 Nothing	-> True)
	$ flattenTSum clo

-- Forall ------------------------------------------------------------------------------------------
-- | Add some forall bindings to the front of this type, 
--	new quantified vars go at front of list.
makeTForall_front :: [(Var, Kind)] -> Type -> Type
makeTForall_front vks tt
 = makeTForall_front' (reverse vks) tt
 
makeTForall_front' vks tt
	| []		<- vks
	= tt

	| (v, k) : rest	<- vks
	= makeTForall_front' rest (TForall (BVar v) k tt)


-- | Add some forall bindings to the front of this type,
--	new quantified vars go at back of list.
makeTForall_back :: [(Var, Kind)] -> Type -> Type
makeTForall_back vks tt
	| []			<- vks
	= tt
	
	| TForall b k t		<- tt
	= TForall b k (makeTForall_back vks t)
	
	| (v, k) : vksRest	<- vks
	= TForall (BVar v) k (makeTForall_back vksRest tt)


-- | Slurp forall bindings that bind vars from this type
slurpVarTForall :: Type -> ([(Bind, Kind)], Type)
slurpVarTForall tt
 = case tt of
 	TForall b@BVar{} k t	
	 -> let	(bksRest, tRest)	= slurpVarTForall t
	    in	( (b, k) : bksRest, tRest)

 	TForall b@BMore{} k t	
	 -> let	(bksRest, tRest)	= slurpVarTForall t
	    in	( (b, k) : bksRest, tRest)
	    
	_ -> ([], tt)


-- | make a chain of KFuns
makeKFuns :: [Kind] -> Kind -> Kind
makeKFuns [] kk	= kk
makeKFuns (k:ks) kk	= KFun k (makeKFuns ks kk)


-- Witnesses ---------------------------------------------------------------------------------------
-- | Make a witness application.
makeTWitness :: TyConWitness -> Kind -> [Type] -> Type
makeTWitness con k ts
	= makeTApp (TCon (TyConWitness con k) : ts)

-- | Take a witness from its constructor application.
--	Returns the witness constructor and its kind, along with the type args.
takeTWitness :: Type -> Maybe (TyConWitness, Kind, [Type])
takeTWitness tt
	| TCon (TyConWitness v k)	<- tt
	= Just (v, k, [])

	| TApp t1 t2		<- tt
	, Just (v, k, ts)	<- takeTWitness t1
	= Just (v, k, ts ++ [t2])
	
	| otherwise
	= Nothing


-- Fetters -----------------------------------------------------------------------------------------	
-- | Wrap a type with some fetters
makeTFetters :: Type -> [Fetter] -> Type
makeTFetters t []	= t
makeTFetters t fs	= TFetters t fs

-- | Take the fetters from a type
takeTFetters :: Type -> [Fetter]
takeTFetters (TFetters t [])	= []
takeTFetters (TFetters t fs)	= fs
takeTFetters t			= []

-- | Add some fetters to a type.
addFetters :: 	[Fetter] -> Type -> Type
addFetters	fsMore	t
 = case t of
	TForall v k x
	 -> TForall v k (addFetters fsMore x)

	TFetters x fs
	 -> case fs ++ fsMore of
	 	[]	-> x
		ff	-> TFetters x ff
	 
	_ -> case fsMore of
		[]	-> t
		ff	-> TFetters t ff

addFetters_front :: [Fetter] -> Type -> Type
addFetters_front fsMore t
 = case t of
	TForall v k x
	 -> TForall v k (addFetters_front fsMore x)

 	TFetters x fs
	 -> case fsMore ++ fs of
	 	[]	-> x
		ff	-> TFetters x ff
		
	_ -> case fsMore of
		[]	-> t
		ff	-> TFetters t ff

-- TFetters vs TConstrain -------------------------------------------------------------------------

-- | Convert top-level occurences of TConstrain to TFetters
toFetterFormT :: Type -> Type
toFetterFormT tt
 = let down = toFetterFormT 
   in case tt of
	TForall    b k t	-> TForall b k (down t)
	TFetters   t fs		-> TFetters (down t) fs

	TConstrain t (Constraints { crsEq, crsMore, crsOther })
	 | Map.null crsEq 
	 , Map.null crsMore
	 , null crsOther
	 -> t
	
	 | otherwise
	 -> TFetters 
		(down t)
		(    [FWhere t1 (down t2) | (t1, t2) <- Map.toList crsEq   ]
		  ++ [FMore  t1 (down t2) | (t1, t2) <- Map.toList crsMore ]
		  ++ crsOther)
			
	TApp t1 t2		-> TApp (down t1) (down t2)
	_			-> tt


-- | Convert top-level occurences of TFetters to TConstrain
toConstrainFormT :: Type -> Type
toConstrainFormT tt
 = let down = toConstrainFormT
   in  case tt of
	TForall    b k t	-> TForall b k (down t)

	TFetters t fs
	 -> let	crsEq		= Map.fromList [(t1, toConstrainFormT t2) | FWhere t1 t2 <- fs]
		crsMore		= Map.fromList [(t1, toConstrainFormT t2) | FMore  t1 t2 <- fs]

		crsOther	= filter (\f -> (not $ isFWhere f) && (not $ isFMore f)) 
				$ map toConstrainFormF fs

	    in	addConstraints (Constraints crsEq crsMore crsOther) (down t)
	
	TConstrain t cs		-> TConstrain (down t) cs
	
	TApp t1 t2		-> TApp (down t1) (down t2)
	TSum k  ts		-> TSum k $ map toConstrainFormT ts
	_			-> tt

toConstrainFormF :: Fetter -> Fetter
toConstrainFormF ff
 = case ff of
	FConstraint c ts	-> FConstraint c $ map toConstrainFormT ts
	FWhere t1 t2		-> FWhere t1     $ toConstrainFormT t2
	FMore  t1 t2		-> FMore  t1	 $ toConstrainFormT t2
	FProj{}			-> ff

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
	
	
-- | Add a new type class context to a type,
--	pushing it under any enclosing foralls.
addContext :: Kind -> Type -> Type
addContext k tt
 = case tt of
 	TForall b@BVar{}  k t	-> TForall b k (addContext k t)
 	TForall b@BMore{} k t	-> TForall b k (addContext k t)
	_			-> TForall BNil k tt

addContexts :: [Kind] -> Type -> Type
addContexts []	   t	= t
addContexts (k:ks) t	
	= TForall BNil k (addContexts ks t)


-- | Create a superkind.
unflattenSuper :: [Kind] -> Super -> Super
unflattenSuper [] 	s	= s
unflattenSuper (k:ks) 	s	= SFun k (unflattenSuper ks s)
	

-- | Strip off TForalls, TFetters and TContext of a type to get the body of the type.
stripToBodyT :: Type -> Type
stripToBodyT tt
 = case tt of
 	TForall  v k t		-> stripToBodyT t
	TFetters t fs		-> stripToBodyT t
	_			-> tt
