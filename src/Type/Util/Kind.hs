
module Type.Util.Kind
	-- namespace things
	( defaultKindV
	, spaceOfKind
	, kindOfSpace 

	-- witnesses
	, makeKWitJoin

	, makeKFun

	, tyConKind
	, takeKindOfType
	, kindOfType_orDie
	, resultKind
	, makeDataKind
	, kindOfType_freakouts)

where

import Type.Exp
import Type.Pretty
import Shared.Var		(NameSpace(..))
import Shared.Error
import Shared.Pretty
import qualified Shared.Var as Var
import Util

import Main.Arg
import qualified Debug.Trace

stage	= "Type.Util.Kind"


-- Namespace things --------------------------------------------------------------------------------
defaultKindV ::	Var	-> Kind
defaultKindV	v
 = case Var.nameSpace v of
 	NameType	-> KValue
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	

-- | Get the namespace associated with a kind.
spaceOfKind ::	Kind -> NameSpace
spaceOfKind  kind
 = case resultKind kind of
 	KValue		-> NameType
	KRegion		-> NameRegion
	KEffect		-> NameEffect
	KClosure	-> NameClosure
	_		-> panic stage
			$ "spaceOfKind: no space for " % kind

-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Kind
kindOfSpace space
 = case space of
 	NameType	-> KValue
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	NameClass	-> KFetter
	_		-> panic stage
			$  "kindOfSpace: no match for " % show space


-- Witnesses ---------------------------------------------------------------------------------------
-- | Join some kind classes
makeKWitJoin :: [Kind] -> Kind
makeKWitJoin ts
 = case ts of
 	[t]	-> t
	ts	-> KWitJoin ts

-- | make a function kind
makeKFun :: [Kind] -> Kind
makeKFun [k]		= k
makeKFun (k : ks)	= KFun k (makeKFun ks)


-- | Take the kind of a tycon
tyConKind :: TyCon -> Kind
tyConKind tyCon
 = case tyCon of
	TyConFun			-> KFun KValue (KFun KValue (KFun KEffect (KFun KClosure KValue)))
	TyConData { tyConDataKind }	-> tyConDataKind
	TyConClass { tyConClassKind }	-> tyConClassKind	 


-- | Get the kind of a type, or die if there is a kind error.
--	This is harder to debug with...
kindOfType_orDie :: Type -> Kind
kindOfType_orDie tt
 = case takeKindOfType tt of
 	Just k		-> k
	Nothing		-> panic stage
			$ "kindOfType: no match for " % tt % "\n"
			%> show tt

-- | Get the kind of a type
--	A types include TApp, they might have internal errors where no kind is extractable.
--	In this case we freakout and return Nothing.
--
takeKindOfType :: Type -> Maybe Kind
takeKindOfType tt
 {- = Debug.Trace.trace
 	( pprStr [PrettyTypeKinds] $ "takeKindOfType " % tt)
 	$ -} =  takeKindOfType' tt

takeKindOfType' tt
 = case tt of
 	TForall v k t	-> takeKindOfType t
	TFetters t fs	-> takeKindOfType t
	
	TSum k ts	-> Just k
	TMask k _ _	-> Just k
	TVar k v	-> Just k
	TTop k		-> Just k
	TBot k		-> Just k
	
	TApp t1 t2
	 -> do	k1	<- takeKindOfType t1
	 	k2	<- takeKindOfType t2
		
		case k1 of
		 KFun k11 k12
		  | k2 == k11	-> return k12
		 
		 _		-> kindOfType_freakout t1 k1 t2 k2
	              
	
	TCon tc		-> Just $ tyConKind tc

	TData k v ts	
	 -> do	ks	<- sequence $ map takeKindOfType ts
	 	
		case appKinds k ks of
		 Just k'	-> return k'
		 _		-> kindOfType_freakouts (TVar k v) k $ zip ts ks
	 
	TFun{}		-> Just KValue
	
	TEffect{}	-> Just KEffect
	
	TFree{}		-> Just KClosure
	TDanger{}	-> Just KClosure
	TTag{}		-> Just KClosure
	
	TWild k		-> Just k
	TClass k cid	-> Just k

	TElaborate e t	-> takeKindOfType t

	TError k t	-> Just k 
	
	_		
	 -> freakout stage 
		("takeKindOfType: no match for " % tt % "\n") 
		Nothing

kindOfType_freakout t1 k1 t2 k2
 = freakout stage	
	( "takeKindOfType: kind error in type application (t1 t2)\n"
	% "    t1  = " % t1 	% "\n"
	% "  K[t1] = " % k1	% "\n"
	% "\n"
	% "    t2  = " % t2 	% "\n"
	% "  K[t2] = " % k2	% "\n")
	Nothing


kindOfType_freakouts t1 k1 tks
 = freakout stage	
	( "takeKindOfType: kind error in type application t1 ts\n"
	% "    t1   = " % t1 	% "\n"
	% "  K[t1]  = " % k1	% "\n"
	% "\n"
	% "    tks  = " % tks 	% "\n")
	Nothing


-- | Apply some kinds to a kind function
--	If this results in a kind error then return Nothing
appKinds :: Kind -> [Kind] 	-> Maybe Kind

appKinds k []		= Just k

appKinds (KFun k1 k2) (k:ks)
	| k1 == k	= appKinds k2 ks
	
appKinds  k ks		= Nothing


-- | Get the result of applying all the paramters to a kind.
resultKind :: Kind -> Kind
resultKind kk
 = case kk of
 	KFun k1 k2	-> resultKind k2
	_		-> kk

-- Make a kind from the parameters to a data type
makeDataKind :: [Var] -> Kind
makeDataKind vs
 	= foldl (flip KFun) KValue 
	$ map (\v -> kindOfSpace (Var.nameSpace v)) 
	$ reverse vs

