
module Type.Util.Kind
	-- namespace things
	( defaultKindV
	, spaceOfKind
	, kindOfSpace 

	-- projections
	, tyConKind

	-- witnesses
	, inventWitnessOfClass

	-- kind functions
	, makeKFun
	, makeKApps
	, makeKSum
	, takeKApps
	, resultKind
	, makeDataKind

	-- kind reconstruction
	, kindOfType
	, kindOfType_orDie
	
	-- fast kind utils
	, isClosure)
where
import Type.Pretty		()
import Type.Builtin
import Type.Util.Bits
import Type.Exp
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import Data.List

stage		= "Type.Util.Kind"

-- Namespace things --------------------------------------------------------------------------------
defaultKindV ::	Var	-> Kind
defaultKindV	v
 = case varNameSpace v of
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
	TyConFun				
	 -> KFun kValue (KFun kValue (KFun kEffect (KFun kClosure kValue)))

	TyConData    { tyConDataKind }		
	 -> tyConDataKind

	TyConWitness { tyConWitnessKind }
	 -> tyConWitnessKind	 


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


-- | Flatten out a dependent kind application into its parts.
takeKApps :: Kind -> Maybe (Kind, [Type])
takeKApps kk
 = case kk of
	KCon{} -> Just (kk, [])

	KApp k1 t2
	  -> let Just (k1', ts)	= takeKApps k1
	     in	 Just (k1', ts ++ [t2])
	
	_ -> Nothing


-- | Make a dependent kind application from a list of types.
makeKApps :: Kind -> [Type] -> Kind
makeKApps k []	= k
makeKApps k ts	= makeKApps' k $ reverse ts

makeKApps' k tt
 = case tt of
	t : []	-> KApp k t
	t : ts	-> KApp (makeKApps' k ts) t 

-- Make a kind from the parameters to a data type
makeDataKind :: [Var] -> Kind
makeDataKind vs
 	= foldl (flip KFun) kValue 
	$ map (\v -> kindOfSpace (varNameSpace v)) 
	$ reverse vs


-- | Join some kind classes
makeKSum :: [Kind] -> Kind
makeKSum ts
 = case nub ts of
 	[t]	-> t
	ts	-> KSum ts


-- Witnesses ---------------------------------------------------------------------------------------

-- | Invent a place-holder witness that satisfies a type class constraint.
--	This is used in Desugar.ToCore when we don't know how to properly construct the
--	real witnesses yet.
inventWitnessOfClass :: Kind -> Maybe Type
inventWitnessOfClass k
	| Just (KCon kiCon s, ts)	<- takeKApps k
	, Just tcWitness		<- takeTyConWitnessOfKiCon kiCon
	= let 	-- Get the kinds of the type arguments.
		Just ks = sequence $ map kindOfType ts

		-- The resulting kind guarantees the constraint.
		kResult	= makeKApps (KCon kiCon s) (zipWith TIndex ks $ reverse [0 .. length ks - 1])
		k'	= makeKFuns ks kResult
		tyCon	= TyConWitness tcWitness k'

		witness	= makeTApp (TCon tyCon : ts)

   	   in	Just witness

inventWitnessOfClass k
	= freakout stage
		("inventWitnessOfClass: don't know how to build witness for '" % k % "'\n")
		Nothing


-- Kind reconstruction -----------------------------------------------------------------------------
-- | Determine the kind of a type.
--   Kinds are often cached in nodes of the type, so we don't have to
--   inspect a whole type to determine its kind. This makes things faster,
--   but isn't a full check. 
--
--   TODO: add checkedKindOfType to do the checks as well.
--
kindOfType :: Type -> Maybe Kind
kindOfType tt 
 = let 	kind	= kindOfType' tt
   in	Just $ kind

kindOfType' tt
 = case tt of
	TClass k _		-> k
	TVar k _		-> k
	TVarMore k _ _		-> k
	TIndex k _		-> k
	TCon tyCon		-> (tyConKind tyCon)
	TBot k			-> k
	TTop k			-> k
	TSum  k _		-> k

	TApp t1 t2		
	 | KFun k11 k12		<- kindOfType' t1
	 -> betaTK 0 t2 k12
	
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
				
	-- some of the helper constructors don't have real kinds ------------
	_			-> panic stage $ "kindOfType bad kind for: " % tt


kindOfType_orDie :: Type -> Kind
kindOfType_orDie tt
 = let  Just k	= kindOfType tt
   in	k

-- Beta --------------------------------------------------------------------------------------------
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
	TContext k t	-> TContext k (down t)
	TFetters t fs	-> TFetters (down t) fs
	TApp t1 t2	-> TApp (down t1) (down t2)
	TSum k ts	-> TSum k (map down ts)
	TCon{}		-> tt
	TVar{}		-> tt
	TVarMore{}	-> tt

	TIndex k ix
	 | ix == depth	-> down tX
	 | otherwise	-> tt
	 	
	TTop{}		-> tt
	TBot{}		-> tt
	TEffect v ts	-> TEffect v (map down ts)
	TFree v t	-> TFree v (down t)

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

