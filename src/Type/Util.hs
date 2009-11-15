module Type.Util
	( module Type.Util.Bits
	, module Type.Util.Instantiate
	, module Type.Util.Elaborate
	, module Type.Util.Kind
	, module Type.Util.Normalise
	, module Type.Util.Pack
	, module Type.Util.Substitute
	, module Type.Util.StripFetters
	, module Type.Util.Unify 
	, module Type.Util.Mask
	, module Type.Util.Trim
	, module Type.Util.Finalise
	, module Type.Util.Quantify
	, module Type.Util.Flatten
	, module Type.Util.JoinSum
	, makeOpTypeT
	, makeTVar 
	, makeTWhere
	, slurpVarsRD)
	
where

import Type.Util.Bits
import Type.Util.Instantiate
import Type.Util.Elaborate
import Type.Util.Kind
import Type.Util.Normalise
import Type.Util.Pack
import Type.Util.Substitute
import Type.Util.StripFetters
import Type.Util.Unify
import Type.Util.Mask
import Type.Util.Trim
import Type.Util.Finalise
import Type.Util.Quantify
import Type.Util.Flatten
import Type.Util.JoinSum

import Type.Exp
import qualified Shared.Var	as Var
import Shared.VarPrim
import Shared.Error
import Util

stage	= "Type.Util"

-- | Make an operational type.
makeOpTypeT :: Type -> Maybe Type
makeOpTypeT tt
 = case tt of
 	TForall v k t		-> makeOpTypeT t
	TFetters t fs		-> makeOpTypeT t
	TFun t1 t2 eff clo	
	 -> case (makeOpTypeT2 t1, makeOpTypeT t2) of
	 	(Just t1', Just t2')	-> Just $ TFun t1' t2' (TBot kEffect) (TBot kClosure)
		_			-> Nothing
		
	TData{}			-> makeOpTypeData tt
	TVar{}			-> Just $ TData kValue primTObj []
	TElaborate ee t		-> makeOpTypeT t
	_			-> freakout stage
					("makeOpTypeT: can't make operational type from " % show tt)
					Nothing
makeOpTypeT2 tt
 = case tt of
 	TForall v k t		-> makeOpTypeT2 t
	TFetters t fs		-> makeOpTypeT2 t
	TVar{}			-> Just $ TData kValue primTObj   []
	TFun{}			-> Just $ TData kValue primTThunk []
	TData{}			-> makeOpTypeData tt
	TElaborate ee t		-> makeOpTypeT t
	_			-> freakout stage
					("makeOpType: can't make operational type from " % show tt)
					Nothing

makeOpTypeData (TData k v ts)
	| last (Var.name v) == '#'
	= case (sequence $ (map makeOpTypeT [t | t <- ts, kindOfType_orDie t == kValue])) of
		Just ts'	-> Just $ TData kValue v ts'
		_		-> Nothing
	
	| otherwise
	= Just $ TData kValue primTObj []

makeOpTypeData _	= Nothing


-- | Make a TVar, using the namespace of the var to determine it's kind
makeTVar :: Var -> Type
makeTVar v	= TVar (kindOfSpace $ Var.nameSpace v) v


-- | Add some where fetters to this type
makeTWhere ::	Type	-> [(Var, Type)] -> Type
makeTWhere	t []	= t
makeTWhere	t vts	
	= TFetters t 
	$ [ FWhere (TVar (defaultKindV v) v) t'
		| (v, t')	<- vts ]


-- Slurping ----------------------------------------------------------------------------------------
-- | Slurp out the region and data vars present in this type
--	Used for crushing ReadT, ConstT and friends
slurpVarsRD
	:: Type 
	-> ( [Region]	-- region vars and cids
	   , [Data])	-- data vars and cids

slurpVarsRD tt
 	= slurpVarsRD_split [] [] 
	$ slurpVarsRD' tt

slurpVarsRD_split rs ds []	= (rs, ds)
slurpVarsRD_split rs ds (t:ts)
 = case t of
 	TVar	k _	| k == kRegion	-> slurpVarsRD_split (t : rs) ds ts
	TClass	k _	| k == kRegion	-> slurpVarsRD_split (t : rs) ds ts

 	TVar	k _	| k == kValue	-> slurpVarsRD_split rs (t : ds) ts
	TClass	k _	| k == kValue	-> slurpVarsRD_split rs (t : ds) ts
	
	_				-> slurpVarsRD_split rs ds ts
	
slurpVarsRD' tt
	| TFetters t f	<- tt
	= slurpVarsRD' t

	| TApp{}	<- tt
	, Just (v, k, ts)	<- takeTData tt
	= catMap slurpVarsRD' ts
	
	| TApp{}	<- tt
	, Just _		<- takeTFun tt
	= []
	
	| TApp t1 t2	<- tt
	= slurpVarsRD' t1 ++ slurpVarsRD' t2

	| TSum{}	<- tt	= []
	| TCon{}	<- tt	= []

	| TVar k _	<- tt
	= if k == kRegion || k == kValue
		then [tt]
		else []
		
	| TVarMore k _ _ <- tt
	= if k == kRegion || k == kValue
		then [tt]
		else []

	| TTop{}	<- tt	= []
	| TBot{}	<- tt	= []

	| TEffect{}	<- tt	= []
	| TFree{}	<- tt	= []
	| TDanger{}	<- tt	= []

	| TData k v ts	<- tt	
	= catMap slurpVarsRD' ts

	| TFun{}	<- tt	= []

	| TClass k _	<- tt
	= if k == kRegion || k == kValue
		then [tt]
		else []
		
	| TError k t	<- tt	
	= []

	| otherwise
	= panic stage
	$  "slurpVarsRD: no match for " % tt



