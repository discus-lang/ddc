
-- | Bits and pieces for working with types.
module Type.Util.Bits
	-- simple
	( takeValueArityOfType
	
	-- projections
	, takeBindingVarF
	
	-- crushing
	, crushT

	-- sums
	
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
import Type.Plate.Trans
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set


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
	TError{}	-> Nothing
	

-- | Take the binding var from FLet's 
takeBindingVarF :: Fetter -> Maybe Var
takeBindingVarF ff
 = case ff of
 	FWhere (TVar k (UVar v)) t2	-> Just v
	_				-> Nothing


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

 where
	isFWhere ff
 	 = case ff of
 		FWhere{}	-> True
		_ 		-> False

	isFMore :: Fetter -> Bool
	isFMore ff
 	 = case ff of
 		FMore{}		-> True
		_ 		-> False


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
