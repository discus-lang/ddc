{-# OPTIONS -fwarn-incomplete-patterns #-}

module Core.ReconKind
	( kindOfType
	, buildWitnessOfClass)
 where

import Core.Exp
import Core.Pretty
import Core.Util.Bits
import Type.Util

import Shared.VarPrim
import Shared.Error

import Util

stage	= "Core.ReconKind"

{-
-- | Take the kind of a tycon
tyConKind :: TyCon -> Kind
tyConKind tyCon
 = case tyCon of
	TyConFun			-> KFun KValue (KFun KValue (KFun KEffect (KFun KClosure KValue)))
	TyConData { tyConDataKind }	-> tyConDataKind
	TyConClass { tyConClassKind }	-> tyConClassKind	 
-}
	
-- | Reconstruct the kind of this type, kind checking along the way
kindOfType :: Type -> Maybe Kind
kindOfType tt

	-- we never actually need the kind of the quantified variable,
	--	so there is no need to add it to the type environment.
	| TForall  b t1 t2	<- tt	= kindOfType t2

	| TContext t1 t2	<- tt	= kindOfType t2
	| TFetters t1 _		<- tt	= kindOfType t1
	
	-- we'll just assume kind annots on TSum and TMask are right, and save
	--	having to check all the elements
	| TSum  k _		<- tt	= Just k
	| TMask k _ _		<- tt	= Just k

	| TVar  k _		<- tt	= Just k
	| TVarMore k _ _	<- tt	= Just k
	| TCon tyCon		<- tt	= Just (tyConKind tyCon)
	| TBot k		<- tt	= Just k
	| TTop k		<- tt	= Just k

	-- application of KForall
	| TApp t1 t2			<- tt
	, Just (KForall k11 k12)	<- kindOfType t1
	, Just k2			<- kindOfType t2
	, k11 == k2
	= Just (betaTK 0 t2 k12)

	-- application of kind function (which is a sugared KForall)
	| TApp t1 t2			<- tt
	, Just (KFun k11 k12)		<- kindOfType t1
	, Just k2			<- kindOfType t2
	, k11 == k2
	= Just k12

	-- application failed.. :(
	| TApp t1 t2			<- tt
	= freakout stage 
		("kindOfType: kind error in type application (t1 t2)\n"
		% "    t1  = " % t1 % "\n"
		% "  K[t1] = " % kindOfType t1	% "\n"
		% "\n"
		% "    t2  = " % t2 % "\n"
		% "  K[t2] = " % kindOfType t2	% "\n")
		Nothing

	-- effect and closure constructors should always be fully applied.
	| TEffect{}		<- tt	= Just KEffect
	| TFree{}		<- tt	= Just KClosure

	| TWild k		<- tt	= Just k
		
	-- some of the helper constructors don't have real kinds
	| otherwise
	= freakout stage 
		("kindOfType: cannot get kind for " % show tt % "\n")
		Nothing


-- Beta --------------------------------------------------------------------------------------------
-- de bruijn style beta evalation
--	used to handle substitution arrising from application of KForall's in kindOfType.

betaTK :: Int -> Type -> Kind -> Kind
betaTK depth tX kk
 = let down 	= betaTK depth tX
   in case kk of
 	KNil		-> kk
	KForall k1 k2	-> KForall k1 (betaTK (depth + 1) tX k2)
	KFun	k1 k2	-> KFun (down k1) (down k2)
	KValue		-> kk
	KRegion		-> kk
	KEffect		-> kk
	KClosure	-> kk
	KClass v ts	-> KClass v (map (betaTT depth tX) ts)
	KWitJoin ks	-> kk

	_	-> panic stage
		$ "betaTK: no match for " % kk
		
	
betaTT :: Int -> Type -> Type -> Type
betaTT depth tX tt
 = let down	= betaTT depth tX
   in  case tt of
   	TNil		-> tt
	TForall b k t	-> TForall b k (down t)
	TContext k t	-> TContext k (down t)
	TFetters t fs	-> TFetters (down t) fs
	TApp t1 t2	-> TApp (down t1) (down t2)
	TSum k ts	-> TSum k (map down ts)
	TMask k t1 t2	-> TMask k (down t1) (down t2)
	TCon{}		-> tt
	TVar{}		-> tt
	TVarMore{}	-> tt

	TIndex ix
	 | ix == depth	-> tX
	 | ix > depth	-> TIndex (ix - 1)
	 | otherwise	-> tt
	 	
	TTop{}		-> tt
	TBot{}		-> tt
	TEffect v ts	-> TEffect v (map down ts)
	TFree v t	-> TFree v (down t)
	TTag{}		-> tt

	TWitJoin ts	-> TWitJoin (map down ts)
	TWild k		-> tt

	_	-> panic stage
		$ "betaTT: no match for " % tt

-- | Invent an explicit witness needed to satisfy a certain constraint
--	This is used in Desugar.ToCore to invent witnesses we don't know how to properly construct yet.
buildWitnessOfClass :: Kind -> Maybe Type
buildWitnessOfClass (KClass v ts)
 = let 	Just ks	= sequence $ map kindOfType ts
	kResult	= KClass v (map TIndex $ reverse [0 .. length ks - 1])
	k	= makeKForall ks kResult
   in	Just (makeTApp (TCon (TyConClass v k) : ts))

buildWitnessOfClass k
	= freakout stage
		("buildWitnessOfClass: don't know how to build witness for '" % k % "'\n")
		Nothing

		

	-- all the types being joined need to be purify witnesses
{-
	TPurifyJoin ts
	 -> let	ks	= map kindOfType ts

		takePureEff (KClass v [eff])
			| v == primPure
			= eff
		
		takePureEff _
			= panic stage $ "kindOfType: takePureEff"
		
		effs	= map takePureEff ks
		
	    in	KClass primPure [makeTSum KEffect effs]

	TPurify eff@(TEffect vE [TVar KRegion vR1]) wit
		|  KClass vC [TVar KRegion vR2]	<- kindOfType wit
		,  vR1 == vR2
		,  vE  == primRead
		,  vC  == primConst
		-> KClass primPure [eff]
-}	
--	TWitJoin ts
--		-> makeKWitJoin (map kindOfType ts)

