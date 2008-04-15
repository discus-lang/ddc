{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Util.Bits
	( pure
	, empty

	, isFConstraint
	
	, crushT
	, makeTSum,	flattenTSum
	, makeTMask,	applyTMask
	, makeTApp
	, makeTData,	takeTData

	, makeTForall
	, makeTForall_back

	, makeTFunEC 
	, addFetters,	addFetters_front
	, takeBindingVarF

	, makeOpTypeT 
	, makeTVar
	, takeCidOfTClass
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
import Type.Util.Kind

-----
stage	= "Type.Util.Bits"

pure	= TBot KEffect
empty	= TBot KClosure

-- 
isFConstraint ff
 = case ff of
 	FConstraint v ts	-> True
	_			-> False


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

	TFree v (TFree v' t)	
	 -> TFree v t

	TFree v (TSum KClosure ts)
	 -> makeTSum KClosure $ map (TFree v) ts

	TMask k1 (TMask k2 t1 t2) t3
	 | k1 == k2
	 -> TMask k1 t1 (TSum k1 [t2, t3])

	TMask k t1 t2
	 -> applyTMask tt

	_	-> tt


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
-- Masks
--
makeTMask :: Kind -> Type -> Type -> Type
makeTMask k t1 t2
 = applyTMask $ case crushT t2 of
 	TBot KClosure	-> t1
	_		-> TMask k t1 t2


-- | Crush a TMask by discarding TFree and TEffects 
--	in the first term which are present in the second.
applyTMask :: Type -> Type
applyTMask tt@(TMask k t1 t2)
 = let	vsKill	= map (\t -> case t of
 				TFree v _	-> v
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


-- | Make a type application
makeTApp :: [Type] -> Type
makeTApp ts = makeTApp' $ reverse ts

makeTApp' xx
 = case xx of
	[]		-> panic stage $ "makeTApp': empty list"
 	x : []		-> x
	x1 : xs		-> TApp (makeTApp' xs) x1
	

-- | Make a data type
makeTData :: Var -> Kind -> [Type] -> Type
makeTData v k ts
 = makeTApp (TCon TyConData { tyConName = v, tyConKind = k } : ts )
	
-- | take a data type
takeTData :: Type -> Maybe (Var, Kind, [Type])
takeTData tt
 = case tt of
	TData k v ts
		-> Just (v, k, ts)

 	TCon TyConData { tyConName = v, tyConKind = k }
		-> Just (v, k, [])
		
	TApp t1 t2
	 -> case takeTData t1 of
	 	Just (v, k, ts)	-> Just (v, k, ts ++ [t2])
		Nothing		-> Nothing
		
	_ -> Nothing


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
makeTFunEC	_   _   []			= panic stage $ "makeTFunEC: not enough args for function"



	
	
-- | Add some fetters to a type.
addFetters :: 	[Fetter] -> Type -> Type
addFetters	fsMore	t
 = case t of
	TForall vks x
	 -> TForall vks (addFetters fsMore x)

	TFetters fs  x	
	 -> case fs ++ fsMore of
	 	[]	-> x
		ff	-> TFetters (nub ff) x
	 
	_ -> case fsMore of
		[]	-> t
		ff	-> TFetters (nub ff) t

addFetters_front :: [Fetter] -> Type -> Type
addFetters_front fsMore t
 = case t of
	TForall vks x
	 -> TForall vks (addFetters_front fsMore x)

 	TFetters fs x
	 -> case fsMore ++ fs of
	 	[]	-> x
		ff	-> TFetters (nub ff) x
		
	_ -> case fsMore of
		[]	-> t
		ff	-> TFetters (nub ff) t

-- | Take the fetters of this type



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
	 	(Just t1', Just t2')	-> Just $ TFun t1' t2' (TBot KEffect) (TBot KClosure)
		_			-> Nothing
		
	TData{}			-> makeOpTypeData tt
	TElaborate ee t		-> makeOpTypeT t
	_			-> freakout stage
					("makeOpType: can't make operational type from " % show tt)
					Nothing
makeOpTypeT2 tt
 = case tt of
 	TForall vks t		-> makeOpTypeT2 t
	TFetters fs t		-> makeOpTypeT2 t
	TVar{}			-> Just $ TData KValue primTObj   []
	TFun{}			-> Just $ TData KValue primTThunk []
	TData{}			-> makeOpTypeData tt
	TElaborate ee t		-> makeOpTypeT t
	_			-> freakout stage
					("makeOpType: can't make operational type from " % show tt)
					Nothing

makeOpTypeData (TData k v ts)
	| last (Var.name v) == '#'
	= case (sequence $ (map makeOpTypeT [t | t <- ts, kindOfType_orDie t == KValue])) of
		Just ts'	-> Just $ TData KValue v ts'
		_		-> Nothing
	
	| otherwise
	= Just $ TData KValue primTObj []

makeOpTypeData _	= Nothing


-- | Make a TVar, using the namespace of the var to determine it's kind

makeTVar :: Var -> Type
makeTVar v	= TVar (kindOfSpace $ Var.nameSpace v) v


takeCidOfTClass :: Type -> Maybe ClassId
takeCidOfTClass (TClass k cid)	= Just cid
takeCidOfTClass _		= Nothing


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

 	TVar   KValue _		-> slurpVarsRD_split rs (t : ds) ts
	TClass KValue _		-> slurpVarsRD_split rs (t : ds) ts
	
	_			-> slurpVarsRD_split rs ds ts
	
slurpVarsRD' tt
 = case tt of
	TFun{}			-> []
 	TData k v ts		-> catMap slurpVarsRD' ts

	TVar KRegion _		-> [tt]
	TVar KValue   _		-> [tt]
	TVar _  _		-> []
	
	TClass KRegion _	-> [tt]	
	TClass KValue   _	-> [tt]
	TClass _ _		-> []

	TFetters fs t		-> slurpVarsRD' t

	TError k t		-> []

	_ 	-> panic stage
		$  "slurpVarsRD: no match for " % tt % "\n"


