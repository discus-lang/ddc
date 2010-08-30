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
	, kindOfType
	, applyKT
	, uncheckedApplyKT)
where
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Var
import {-# SOURCE #-} DDC.Type.Pretty

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
-- | Quickly determine the kind of a type.
--   This does not real checking, so will not catch all errors involving malformed types.
--   If you want that then use the "DDC.Core.Lint" modules.
kindOfType :: Type -> Kind
kindOfType tt
 = case tt of
	TNil{}			-> panic stage $ "kindOfType: no kind for TNil"
	TVar k _		-> k
	TCon tyCon		-> (tyConKind tyCon)
	TSum  k _		-> k
	TApp t1 t2		-> uncheckedApplyKT (kindOfType t1) t2	
	TForall  _ _ t2		-> kindOfType t2
	TConstrain t1 _		-> kindOfType t1
	TError k _		-> k
	


-- Kind/Type application --------------------------------------------------------------------------
-- | Apply a type to a kind, yielding a new kind.
--   This performs a de Bruijn beta reduction, substituting types for indices.
applyKT :: Kind -> Type -> Kind
applyKT k1 t2
	= applyKT_check True k1 t2


-- | Like `applyKT` but don't check that the argument has the right kind, which is a bit faster.
uncheckedApplyKT :: Kind -> Type -> Kind
uncheckedApplyKT k1 t2
	= applyKT_check False k1 t2
	
		
applyKT_check :: Bool -> Kind -> Type -> Kind
applyKT_check check k1 t2
 = case k1 of
	KFun k11 k12
	 | not check
	 -> applyKT' 0 k12 t2

	 | k2	<- kindOfType t2
	 , k2 == k11 
	 -> applyKT' 0 k12 t2
	  
	 -- TODO: We're not checking that k2 is really a kBox kind.
	 | _k2	<- kindOfType t2
	 , k11 == kBox
	 -> applyKT' 0 k12 t2
	      
	_ -> freakout stage 
	    (vcat 
		[ ppr "Kind error in (kind/type) application."
		, " cannot apply type: "	% prettyType t2 
		, ppr "       to kind: "	% prettyKind k1
		, ppr "in application:\n"	%> prettyKind (KApp k1 t2)])
	   $ KNil

applyKT' depth kk tX
 = case kk of
 	KNil			-> kk
	KCon{}			-> kk
	KFun k1 k2		-> KFun (applyKT' depth k1 tX) (applyKT' (depth + 1) k2 tX)
	KApp k t		-> KApp (applyKT' depth k  tX) (applyKT_type depth tX t)
	KSum ks			-> KSum $ map (flip (applyKT' depth) tX) ks
		

applyKT_type :: Int -> Type -> Type -> Type
applyKT_type depth tX tt
 = let down	= applyKT_type depth tX
   in  case tt of
   	TNil			-> tt
	TForall b k t		-> TForall b k (down t)
	TConstrain t crs	-> TConstrain  (down t) crs
	TApp t1 t2		-> TApp (down t1) (down t2)
	TSum k ts		-> TSum k (map down ts)
	TCon{}			-> tt

	TVar _ (UIndex ix)
	 | ix == depth		-> down tX
	 | otherwise		-> tt

	TVar{}			-> tt
	TError{}		-> tt

