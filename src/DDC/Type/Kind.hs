{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Utils concerning kinds
module DDC.Type.Kind
	( -- * Namespaces
	  spaceOfKind
	, kindOfSpace 
	, defaultKindOfVar

	-- * Fast kind tests
	, isValueType,	isValueKind
	, isRegion, 	isRegionKind
	, isEffect,	isEffectKind
	, isClosure,	isClosureKind

	-- * Projections
	, tyConKind

	-- * Kind reconstruction
	, kindOfType)
where
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Var

stage		= "DDC.Type.Kind"

-- Namespace things --------------------------------------------------------------------------------
-- | Get the namespace associated with a kind.
spaceOfKind ::	Kind -> Maybe NameSpace
spaceOfKind  kind
	| kind == kValue	= Just NameType
	| kind == kRegion	= Just NameRegion
	| kind == kEffect	= Just NameEffect
	| kind == kClosure	= Just NameClosure
	| otherwise		= Nothing
	

-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Maybe Kind
kindOfSpace space
 = case space of
 	NameType		-> Just kValue
	NameRegion		-> Just kRegion
	NameEffect		-> Just kEffect
	NameClosure		-> Just kClosure
	_			-> Nothing
	
	
-- | Get the kind associated with the namespace of a var.
defaultKindOfVar :: Var	-> Maybe Kind
defaultKindOfVar v
 = case varNameSpace v of
 	NameType		-> Just kValue
	NameRegion		-> Just kRegion
	NameEffect		-> Just kEffect
	NameClosure		-> Just kClosure
	_			-> Nothing


-- Fast kind utils ---------------------------------------------------------------------------------
-- | Fast test whether a type is a value type.
isValueType :: Type -> Bool
{-# INLINE isValueType #-}
isValueType tt
 = case tt of
	TApp{}			-> isValueKind (kindOfType tt)
	TVar k _		-> isValueKind k
	TCon tc			-> isValueKind (tyConKind tc)
	TSum{}			-> False
	TForall _ _ t1		-> isValueType t1
	TFetters   t1 _		-> isValueType t1
	TConstrain t1 _		-> isValueType t1
	TError k _		-> isValueKind k
	_			-> False
	

-- | Fast test whether a type is a region.
isRegion :: Type -> Bool
{-# INLINE isRegion #-}
isRegion tt
 = case tt of
	TVar k _		-> isRegionKind k
	_			-> False


-- | Fast test whether a type is a closure.
isClosure :: Type -> Bool
{-# INLINE isClosure #-}
isClosure tt
 = case tt of
	TApp{}			-> isClosureKind (kindOfType tt)
 	TSum	 k _		-> isClosureKind k
	TVar	 k _		-> isClosureKind k
	TFetters t1 _		-> isClosure t1
	TConstrain t1 _		-> isClosure t1
	TForall _ _ t1		-> isClosure t1
	_			-> False


-- | Fast test whether a type is an effect.
isEffect :: Type -> Bool
{-# INLINE isEffect #-}
isEffect tt
 = case tt of
	TApp{}			-> isEffectKind (kindOfType tt)
	TSum k _		-> isEffectKind k
	TVar k _		-> isEffectKind k
	TFetters t1 _		-> isEffect t1
	TConstrain t1 _		-> isEffect t1
	TForall _ _ t1		-> isEffect t1
	_			-> False


-- | Fast test whether a kind is the value type kind.
isValueKind :: Kind -> Bool
{-# INLINE isValueKind #-}
isValueKind kk
 = case kk of
	KCon KiConValue _	-> True
	_			-> False


-- | Fast test whether a kind is the region kind.
isRegionKind :: Kind -> Bool
{-# INLINE isRegionKind #-}
isRegionKind kk
 = case kk of
	KCon KiConRegion _	-> True
	_			-> False


-- | Fast test whether an kind is the effect kind.
isEffectKind :: Kind -> Bool
{-# INLINE isEffectKind #-}
isEffectKind kk
 = case kk of
	KCon KiConEffect _	-> True
	_			-> False


-- | Fast test whether a kind is the closure kind.
isClosureKind :: Kind -> Bool
{-# INLINE isClosureKind #-}
isClosureKind kk
 = case kk of
	KCon KiConClosure _	-> True
	_			-> False


-- Projections -------------------------------------------------------------------------------------
-- | Take the kind of a type constructor.
tyConKind :: TyCon -> Kind
tyConKind tyCon
 = case tyCon of
	TyConFun				
	 -> KFun kValue (KFun kValue (KFun kEffect (KFun kClosure kValue)))

	TyConData    { tyConDataKind }		
	 -> tyConDataKind

	TyConEffect { tyConEffectKind }
	 -> tyConEffectKind

	TyConClosure { tyConClosureKind }
	 -> tyConClosureKind

	TyConWitness { tyConWitnessKind }
	 -> tyConWitnessKind	 

	TyConElaborate { tyConElaborateKind }
	 -> tyConElaborateKind


-- Kind reconstruction ----------------------------------------------------------------------------
-- | Determine the kind of a type.
--   Panics on malformed types.
--
kindOfType :: Type -> Kind
kindOfType tt
 = case tt of
	TNil{}			-> panic stage $ "kindOfType: no kind for TNil"
	TVar k _		-> k
	TCon tyCon		-> (tyConKind tyCon)
	TSum  k _		-> k

	TApp t1 t2		
	 | KFun _ k12		<- kindOfType t1
	 -> betaTK 0 t2 k12

	TApp{}			-> panic stage $ "kindOfType: cannot apply " ++ show tt
	
	TForall  _ _ t2		-> kindOfType t2
	TFetters   t1 _		-> kindOfType t1
	TConstrain t1 _		-> kindOfType t1
	TError k _		-> k
		

-- de bruijn style beta evalation
--	used to handle substitution arrising from application of KForall's in kindOfType.
betaTK :: Int -> Type -> Kind -> Kind
betaTK depth tX kk
 = case kk of
 	KNil		-> kk
	KCon{}		-> kk
	KFun k1 k2	-> KFun k1 (betaTK (depth + 1) tX k2)
	KApp k t	-> KApp (betaTK depth tX k) (betaTT depth tX t)
	KSum ks		-> KSum $ map (betaTK depth tX) ks
		
	
betaTT :: Int -> Type -> Type -> Type
betaTT depth tX tt
 = let down	= betaTT depth tX
   in  case tt of
   	TNil		-> tt
	TForall b k t	-> TForall b k (down t)
	TFetters t fs	-> TFetters (down t) fs
	TApp t1 t2	-> TApp (down t1) (down t2)
	TSum k ts	-> TSum k (map down ts)
	TCon{}		-> tt

	TVar _ (UIndex ix)
	 | ix == depth	-> down tX
	 | otherwise	-> tt

	TVar{}		-> tt
	 	
	_		-> panic stage
			$ "betaTT: no match for " % show tt

