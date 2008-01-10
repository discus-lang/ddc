
module Type.Util.Bits
	( pure
	, empty

	, isFConstraint
	
	, crushT
	, makeTSum,	flattenTSum
--	, makeTUnify,	flattenTUnify
	, makeTMask,	applyTMask

	, makeTForall
	, makeTForall_back

	, makeTFunEC 
	, substituteTT
	, substituteVT
	, spaceOfKind
	, kindOfSpace 
	, kindOfType,	takeKindOfType
	, flattenKind 
	, bindFreeVarsT

	, addFetters 
	, takeBindingVarF

	, makeOpTypeT 
	, makeTVar
	, takeCidOfTClass
	
	, snocTFun
	, snocTData
	
	, slurpVarsRD)



where


-----
import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set

-----
import Shared.Error
import qualified Shared.Var as Var
import Shared.Var (Var, NameSpace(..))
import Shared.VarPrim

import Type.Exp
import Type.Plate
import Type.Pretty		()

-----
stage	= "Type.Util.Bits"

pure	= TBot KEffect
empty	= TBot KClosure

-- 
isFConstraint ff
 = case ff of
 	FConstraint v ts	-> True
	_			-> False


-- |
crushT :: Type -> Type
crushT t
 = let	t'	= transformT crushT1 t
   in	if t == t'
   	 then	t
	 else 	crushT t'

crushT1 :: Type -> Type
crushT1 t
 = case t of
 	TSum k ts			
	 -> makeTSum k 		$ flattenTSum t

--	TUnify k ts			
--	 -> makeTUnify k	$ flattenTUnify t

	TFree v (TFree v' t)	
	 -> TFree v t

	TFree v (TSum KClosure ts)
	 -> makeTSum KClosure $ map (TFree v) ts

	TMask k1 (TMask k2 t1 t2) t3
	 | k1 == k2
	 -> TMask k1 t1 (TSum k1 [t2, t3])

	TMask k t1 t2
	 -> applyTMask t
	_				-> t


----------------------
-- Sums

-- | Make a new sum from a list of type, crushing the list and substituting
--	TPure\/TEmpty if there is nothing to sum. If there only one thing
--	after crushing then return that thing instead of a sum.
makeTSum :: Kind -> [Type] -> Type
makeTSum k ts
 = case nub $ catMap flattenTSum ts of
 	[]	-> TBot k
	[t']	-> t'
	ts'	-> TSum k ts'

-- | Crush nested TSums into their components.
flattenTSum :: Type -> [Type]
flattenTSum tt
 = case tt of
	TBot k			-> []
	TSum k ts		-> catMap flattenTSum ts
	TFree v (TBot k)	-> []
	_			-> [tt]


-----------------------
-- Unify

-- | Crush nested TUnifys into their components.
{-flattenTUnify :: Type -> [Type]
flattenTUnify tt
 = case tt of
 	TBot k		-> []
	TUnify k ts	-> catMap flattenTUnify ts
	_		-> [tt]

-- | Make a unify from the list of these types.	
makeTUnify :: Kind -> [Type] -> Type
makeTUnify k ts
 = case nub $ catMap flattenTUnify ts of
 	[]	-> TBot k
	[t]	-> t
	ts'	-> TUnify k ts'
-}

-----------------------
-- Masks
--
makeTMask :: Kind -> Type -> Type -> Type
makeTMask k t1 t2
 = case crushT t2 of
 	TBot KClosure	-> t1
	_		-> TMask k t1 t2


-- | Crush a TMask by discarding TFree and TEffects 
--	in the first term which are present in the second.
applyTMask :: Type -> Type
applyTMask tt@(TMask k t1 t2)
 = let	vsKill	= map (\t -> case t of
 				TFree v t	-> v
				TTag  v		-> v
				_		
				 -> panic stage 
				 	$ "applyTMask: no match for " % show t % "\n"
				  	% "  tt = " % tt % "\n")
		$ flattenTSum t2
		
	tsMasked
		= map (\t -> case t of
				TFree v tr	
				 | v `elem` vsKill	-> TBot k
				 | otherwise		-> TFree v tr
				
				_			-> TMask k t t2)
				
				
		$ flattenTSum t1
		
   in	makeTSum k tsMasked
   
applyTMask tt	= tt


-- | Add some forall bindings to the front of this type, 
--	new quantified vars go at front of list.
makeTForall :: [(Var, Kind)] -> Type -> Type
makeTForall vks tt
	| []	<- vks
	= tt

	| TForall vks' t	<- tt
	= TForall (vks ++ vks') t

	| otherwise
	= TForall vks tt


-- | Add some forall bindings to the front of this type,
--	new quantified vars go at back of list.
makeTForall_back :: [(Var, Kind)] -> Type -> Type
makeTForall_back vks tt
	| []	<- vks
	= tt
	
	| TForall vks' t	<- tt
	= TForall (vks' ++ vks) t
	
	| otherwise
	= TForall vks tt


-----------------------
-- makeTFun / chopTFun
--	Converts a list of types:	[t1, t2, t3, t4]
--	into a function type:		t1 -> (t2 -> (t3 -> t4))
--		and vise versa
--
makeTFunEC ::	Effect -> Closure -> [Type]	-> Type
makeTFunEC	eff clo (x:[])			= x
makeTFunEC	eff clo (x:xs)			= TFun x (makeTFunEC eff clo xs) eff clo


-- | Substitute types for types in some type.
	
substituteTT 
	:: TransM (State ()) a
	=> Map Type Type
	-> a -> a
	
substituteTT sub tt
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


-- | Substitute vars for types in some type

substituteVT 
	:: TransM (State ()) a
	=> Map Var Type
	-> a -> a

substituteVT sub tt
	= transformT (\t -> case t of
			TVar _ v
			 -> case Map.lookup v sub of
			 	Just t'	-> t'
				Nothing	-> t
				
			_	-> t)
	$ tt

-- | Get the namespace associated with a kind.
spaceOfKind ::	Kind -> NameSpace
spaceOfKind  kind
 = case kind of
 	KData		-> NameType
	KRegion		-> NameRegion
	KEffect		-> NameEffect
	KClosure	-> NameClosure

-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Kind
kindOfSpace space
 = case space of
 	NameType	-> KData
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	NameClass	-> KFetter
	_		-> panic stage
			$  "kindOfSpace: no match for " % show space


-- | Get the kind of a type
--	For some constructors, ie TClass, there's no way to work out the kind
--	directly, so kindOfType will fail in this case.
--
kindOfType :: Type -> Kind
kindOfType tt
 = case takeKindOfType tt of
 	Just k		-> k
	Nothing		-> panic stage
			$ "kindOfType: no match for " % show tt

takeKindOfType :: Type -> Maybe Kind
takeKindOfType tt
 = case tt of
 	TForall vks t	-> takeKindOfType t
	TFetters fs t	-> takeKindOfType t
	
--	TUnify k ts	-> Just k
	TSum k ts	-> Just k
	TMask k _ _	-> Just k
	TVar k v	-> Just k
	TTop k		-> Just k
	TBot k		-> Just k
	
	TData{}		-> Just KData
	TFun{}		-> Just KData
	
	TEffect{}	-> Just KEffect
	
	TFree{}		-> Just KClosure
	TTag{}		-> Just KClosure
	
	TWild k		-> Just k
	TClass k cid	-> Just k
	TAccept t	-> takeKindOfType t

	TElaborate t	-> takeKindOfType t
	TMutable t	-> takeKindOfType t

	TError k t	-> Just k 
	_		-> Nothing
	
	
-----
flattenKind ::	Kind	-> [Kind]
flattenKind kk
 = case kk of
 	KFun k1 k2	-> k1 : flattenKind k2
	_		-> [kk]



-----------------------
-- bindFreeVarsT
--	Gather up the list of unbound vars in a type and add explicitly
--	generalise them by adding a forall out the front.
--	
bindFreeVarsT :: 	Type -> Type
bindFreeVarsT	t
 = let	vsFree	= Set.toList $ freeVars t
   in	bindFreeVarsT' 
   		[ (v, kindOfSpace $ Var.nameSpace v) | v <- vsFree ]
		t
	
bindFreeVarsT' vks t
 = case vks of
 	[]	-> t
	_	-> case t of
			TForall vks' x 	-> TForall (vks ++ vks') x
			_		-> TForall vks t


	

-- | Add some fetters to a type.
addFetters :: 	[Fetter] -> Type -> Type
addFetters	fsMore	t
 = case t of
	TFetters fs  x	
	 -> case fs ++ fsMore of
	 	[]	-> x
		ff	-> TFetters (nub ff) x
	 
	_ -> case fsMore of
		[]	-> t
		ff	-> TFetters (nub ff) t


-- | Take the binding var from FLet's 
takeBindingVarF :: Fetter -> Maybe Var
takeBindingVarF ff
 = case ff of
 	FLet (TVar k v) t2	-> Just v
	_			-> Nothing


-- | Make an operational type.
makeOpTypeT :: Type -> Maybe Type
makeOpTypeT tt
 = case tt of
 	TForall vks t		-> makeOpTypeT t
	TFetters fs t		-> makeOpTypeT t
	TFun t1 t2 eff clo	
	 -> case (makeOpTypeT2 t1, makeOpTypeT t2) of
	 	(Just t1', Just t2')	-> Just $ TFun t1' t2' (TTop KEffect) (TTop KClosure)
		_			-> Nothing
		
	TData{}			-> makeOpTypeData tt
	_			-> Nothing

makeOpTypeT2 tt
 = case tt of
 	TForall vks t		-> makeOpTypeT2 t
	TFetters fs t		-> makeOpTypeT2 t
	TVar{}			-> Just $ TData primTObj   []
	TFun{}			-> Just $ TData primTThunk []
	TData{}			-> makeOpTypeData tt
	_			-> freakout stage
					("makeOpType: can't make operational type from " % show tt)
					Nothing

makeOpTypeData (TData v ts)
	| last (Var.name v) == '#'
	= case (sequence $ (map makeOpTypeT [t | t <- ts, kindOfType t == KData])) of
		Just ts'	-> Just $ TData v ts'
		_		-> Nothing
	
	| otherwise
	= Just $ TData primTObj []


-- | Make a TVar, using the namespace of the var to determine it's kind

makeTVar :: Var -> Type
makeTVar v	= TVar (kindOfSpace $ Var.nameSpace v) v


takeCidOfTClass :: Type -> Maybe ClassId
takeCidOfTClass (TClass k cid)	= Just cid
takeCidOfTClass _		= Nothing



snocTFun :: 	Type -> Maybe (Type, Type, Type, Type)
snocTFun	tt
 = case tt of
 	TFun t1 t2 eff clo	-> Just (t1, t2, eff, clo)
	_			-> Nothing
	
snocTData ::	Type -> Maybe (Var, [Type])
snocTData	tt
 = case tt of
 	TData v ts		-> Just (v, ts)
	_			-> Nothing





-- | Slurp out the region and data vars present in this type
--	Used for crushing ReadT, ConstT and friends
slurpVarsRD
	:: Type 
	-> ( [Region]	-- region vars and cids
	   , [Data])	-- data vars and cids

slurpVarsRD tt
 = 	slurpVarsRD_split [] [] $ slurpVarsRD' tt

slurpVarsRD_split rs ds []	= (rs, ds)
slurpVarsRD_split rs ds (t:ts)
 = case t of
 	TVar   KRegion _	-> slurpVarsRD_split (t : rs) ds ts
	TClass KRegion _	-> slurpVarsRD_split (t : rs) ds ts

 	TVar   KData _		-> slurpVarsRD_split rs (t : ds) ts
	TClass KData _		-> slurpVarsRD_split rs (t : ds) ts
	
	_			-> slurpVarsRD_split rs ds ts
	
slurpVarsRD' tt
 = case tt of
	TFun{}			-> []
 	TData v ts		-> catMap slurpVarsRD' ts

	TVar KRegion _		-> [tt]
	TVar KData   _		-> [tt]
	TVar _  _		-> []
	
	TClass KRegion _	-> [tt]	
	TClass KData   _	-> [tt]
	TClass _ _		-> []

	TFetters fs t		-> slurpVarsRD' t

	TError k t		-> []

	_ 	-> panic stage
		$  "slurpVarsRD: no match for " % tt % "\n"
