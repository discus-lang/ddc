
module Type.Util.Kind
	-- namespace things
	( defaultKindV
	, spaceOfKind
	, kindOfSpace 

	-- projections
	, tyConKind

	-- witnesses
	, makeKWitJoin
	, inventWitnessOfClass

	-- kind functions
	, makeKFun
	, resultKind
	, makeDataKind

	-- kind reconstruction
	, kindOfType
	, kindOfType_orDie
	
	-- fast kind utils
	, isClosure)

where
import Type.Pretty
import Type.Util.Bits
import Type.Exp

import Shared.Var		(NameSpace(..))
import Shared.Error
import Shared.Pretty
import qualified Shared.Var as Var

import Util

import Main.Arg
import qualified Debug.Trace	as Debug

-----
stage	= "Type.Util.Kind"
debug	= False
trace ss x	
	= if debug
		then Debug.trace (pprStrPlain ss) x
		else x


-- Namespace things --------------------------------------------------------------------------------
defaultKindV ::	Var	-> Kind
defaultKindV	v
 = case Var.nameSpace v of
 	NameType		-> kValue
	NameRegion		-> kRegion
	NameEffect		-> kEffect
	NameClosure		-> kClosure
	

-- | Get the namespace associated with a kind.
spaceOfKind ::	Kind -> Maybe NameSpace
spaceOfKind  kind
	| kind == kValue	= Just NameType
	| kind == kRegion	= Just NameRegion
	| kind == kEffect	= Just NameEffect
	| kind == kClosure	= Just NameClosure
	| otherwise		= Nothing
	

-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Kind
kindOfSpace space
 = case space of
 	NameType		-> kValue
	NameRegion		-> kRegion
	NameEffect		-> kEffect
	NameClosure		-> kClosure
	NameClass		-> panic stage "kindOfSpace: witness"
	_			-> panic stage
				$  "kindOfSpace: no match for " % show space


-- Projections -------------------------------------------------------------------------------------
-- | Take the kind of a tycon
tyConKind :: TyCon -> Kind
tyConKind tyCon
 = case tyCon of
	TyConFun			-> KFun kValue (KFun kValue (KFun kEffect (KFun kClosure kValue)))
	TyConData { tyConDataKind }	-> tyConDataKind
	TyConClass { tyConClassKind }	-> tyConClassKind	 


-- Kind Functions ----------------------------------------------------------------------------------
-- | Get the result of applying all the paramters to a kind.
resultKind :: Kind -> Kind
resultKind kk
 = case kk of
 	KFun k1 k2	-> resultKind k2
	_		-> kk


-- | make a function kind
makeKFun :: [Kind] -> Kind
makeKFun [k]		= k
makeKFun (k : ks)	= KFun k (makeKFun ks)


-- Make a kind from the parameters to a data type
makeDataKind :: [Var] -> Kind
makeDataKind vs
 	= foldl (flip KFun) kValue 
	$ map (\v -> kindOfSpace (Var.nameSpace v)) 
	$ reverse vs


-- Witnesses ---------------------------------------------------------------------------------------
-- | Join some kind classes
makeKWitJoin :: [Kind] -> Kind
makeKWitJoin ts
 = case ts of
 	[t]	-> t
	ts	-> KWitJoin ts

-- | Invent an explicit witness needed to satisfy a certain constraint
--	This is used in Desugar.ToCore when we don't know how to properly construct our witnesses yet.
inventWitnessOfClass :: Kind -> Maybe Type
inventWitnessOfClass (KClass v ts)
 = let 	Just ks	= sequence $ map kindOfType ts
	kResult	= KClass v (map TIndex $ reverse [0 .. length ks - 1])
	k	= makeKForall ks kResult
   in	Just (makeTApp (TCon (TyConClass v k) : ts))

inventWitnessOfClass k
	= freakout stage
		("inventWitnessOfClass: don't know how to build witness for '" % k % "'\n")
		Nothing


-- Kind reconstruction -----------------------------------------------------------------------------
-- | Reconstruct the kind of this type, kind checking along the way
kindOfType :: Type -> Maybe Kind
kindOfType tt 
 = let 	kind	= kindOfType' tt
   in	trace 	("kindOfType: " % tt 	% "\n")
		$ Just $ kind

kindOfType' tt
 = trace ("kindOfType': " % tt) $ 
   case tt of
	TClass k _		-> k
	TVar k _		-> k
	TVarMore k _ _		-> k
	TCon tyCon		-> (tyConKind tyCon)
	TBot k			-> k
	TTop k			-> k
	
	-- we'll just assume kind annots on TSum and TMask are right, and save
	--	having to check all the elements
	TSum  k _		-> k

	-- application of KForall
	-- TODO: we're not checking the kinds match up atm, it's too much of a perf hit.
	--	 rely on core lint to detect kind problems.
	TApp t1 t2		
	 | KForall k11 k12	<- kindOfType' t1
	 , k2			<- kindOfType' t2
--	 , k11 == k2
	 -> betaTK 0 t2 k12

	-- application of kind function (which is a sugared KForall)
	-- TODO: we're not checking the kinds match up atm, it's too much of a perf hit.
	--	 rely on core lint to detect kind problems.
	TApp t1 t2
	 | KFun k11 k12		<- kindOfType' t1
	 , k2			<- kindOfType' t2
--	 , k11 == k2
	 -> k12

	-- application failed.. :(
	TApp t1 t2
	 -> kindOfType_freakout t1 t2
	
	TForall  b t1 t2	-> kindOfType' t2

	TContext t1 t2		-> kindOfType' t2
	TFetters t1 _		-> kindOfType' t1
	TConstrain t1 crs	-> kindOfType' t1
	
	
	-- effect and closure constructors should always be fully applied.
	TEffect{}		-> kEffect
	TFree{}			-> kClosure
	TDanger{}		-> kClosure

	TError k _		-> k
	TElaborate e t		-> kindOfType' t
	
	-- used in core -----------------------------------------------------
	-- The KJoins get crushed during Core.Util.Pack.packK
	TWitJoin ts
	 | ks			<- map kindOfType' ts
	 -> makeKWitJoin ks
			
	-- some of the helper constructors don't have real kinds ------------
	_			-> panic stage $ "kindOfType bad kind for: " % tt


kindOfType_freakout t1 t2
 = panic stage	
	( "takeKindOfType: kind error in type application (t1 t2)\n"
	% "    t1  = " % t1 	% "\n"
	% "\n"
	% "    t2  = " % t2 	% "\n")
	Nothing

kindOfType_orDie :: Type -> Kind
kindOfType_orDie tt
 = let  Just k	= kindOfType tt
   in	k

-- Beta --------------------------------------------------------------------------------------------
-- de bruijn style beta evalation
--	used to handle substitution arrising from application of KForall's in kindOfType.

betaTK :: Int -> Type -> Kind -> Kind
betaTK depth tX kk
 = trace ("betaTK " % tX % "\n") $
   let down 	= betaTK depth tX
   in case kk of
 	KNil		-> kk
	KForall k1 k2	-> KForall k1 (betaTK (depth + 1) tX k2)
	KFun	k1 k2	-> KFun (down k1) (down k2)
	KCon{}		-> kk
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

	TWitJoin ts	-> TWitJoin (map down ts)

	_	-> panic stage
		$ "betaTT: no match for " % tt


-- Fast kind utils ---------------------------------------------------------------------------------

-- Used in Core.Subsumes
isClosure :: Type -> Bool
isClosure tt
 = case tt of
	-- closures are always fully constructed
	TApp{}			-> False
 	TSum	 k _		-> k == kClosure
	TVar	 k _		-> k == kClosure
	TVarMore k _ _		-> k == kClosure
	TClass	 k _		-> k == kClosure
	TFree{}			-> True
	TDanger{}		-> True
	TTop	k 		-> k == kClosure
	TBot	k 		-> k == kClosure

	TFetters t1 _		-> isClosure t1
	TConstrain t1 _		-> isClosure t1
	TContext   _  t1	-> isClosure t1
	_			-> False

