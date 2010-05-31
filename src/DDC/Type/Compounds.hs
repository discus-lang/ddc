{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Construction and destruction of common compound things.
--	Also known as 'smart' constructors and destructors.
module DDC.Type.Compounds
	( -- * Binds
	  takeVarOfBind

	  -- * Varish things
	, takeTClass

	  -- * General type application
	, makeTApp
	, takeTApps
	
	  -- * Function types.
	, makeTFun
	, makeTFunsPureEmpty
	, makeTFunsEC
	, takeTFun
	, flattenTFuns
	
	  -- * Data types
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
	, takeTFree
	, makeTDanger
	, takeTDanger
	
	  -- * Fetters
	, makeTFetters
	, takeTFetters
	, addFetters
	
	  -- * Constraints
	, makeTConstrain
	
	 -- * Quantification
	, makeTForall_front
	, makeTForall_back
	, takeTForall
	)
where
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Var
import Data.List
import Util
import qualified Data.Map	as Map

stage	= "DDC.Type.Compounds"

-- Binds ------------------------------------------------------------------------------------------
-- | Get the `Var` from a `Bind`, if any.
takeVarOfBind :: Bind -> Maybe Var
takeVarOfBind bb
 = case bb of
	BNil		-> Nothing
 	BVar v		-> Just v
	BMore v _	-> Just v


-- Varish things ----------------------------------------------------------------------------------
-- | Get the `ClassId` from a `TClass`, if any.
takeTClass :: Type -> Maybe ClassId
takeTClass (TVar _ (UClass cid))	= Just cid
takeTClass _				= Nothing


-- General Type Application -----------------------------------------------------------------------
-- | Make a type application. 
--   The first element in the list is the constructor and the rest are its arguments.
makeTApp :: [Type] -> Type
makeTApp ts = makeTApp' $ reverse ts

makeTApp' xx
 = case xx of
	[]		-> panic stage $ "makeTApp': empty list"
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
 	= makeTApp (TCon TyConData { tyConName = v, tyConDataKind = k } : ts )

	
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
	= makeTApp (TCon (TyConWitness con k) : ts)


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
	TApp (TCon (TyConClosure (TyConClosureFree v) _)) t2
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


-- Fetters -----------------------------------------------------------------------------------------	
-- | Wrap a type with some fetters
makeTFetters :: Type -> [Fetter] -> Type
makeTFetters t []	= t
makeTFetters t fs	= TFetters t fs


-- | Take the fetters from a type
takeTFetters :: Type -> [Fetter]
takeTFetters (TFetters _ [])	= []
takeTFetters (TFetters _ fs)	= fs
takeTFetters _			= []


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

-- Constraints ------------------------------------------------------------------------------------
-- | If the given constraints are non empty then add a TConstrain to the type, 
--   otherwise return the original type.
makeTConstrain :: Type -> Constraints -> Type
makeTConstrain tt crs
 = if not (Map.null $ crsEq crs) || not (Map.null $ crsMore crs) || not (null $ crsOther crs)
    	then TConstrain tt crs
	else tt


-- Quantification ---------------------------------------------------------------------------------
-- | Add some forall bindings to the front of this type.
makeTForall_front :: [(Var, Kind)] -> Type -> Type
makeTForall_front vks tt
 = makeTForall_front' (reverse vks) tt
 
makeTForall_front' vks tt
 = case vks of
	[]		-> tt
	(v, k) : rest	-> makeTForall_front' rest (TForall (BVar v) k tt)


-- | Add some forall bindings to this type, placing them after others already there.
makeTForall_back :: [(Var, Kind)] -> Type -> Type
makeTForall_back [] tt			= tt
makeTForall_back vks@((v, k) : vksRest) tt
 = case tt of
	TForall b k' t
	  -> TForall b k' (makeTForall_back vks t)
	
	_ -> TForall (BVar v) k (makeTForall_back vksRest tt)


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
