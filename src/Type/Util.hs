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
	, module Type.Util.Trim
	, module Type.Util.Finalise
	, module Type.Util.Quantify
	, module Type.Util.Flatten
	, makeOpTypeT
	, makeTVar )
	
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
import Type.Util.Trim
import Type.Util.Finalise
import Type.Util.Quantify
import Type.Util.Flatten


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
	 	(Just t1', Just t2')	-> Just $ TFun t1' t2' (TBot KEffect) (TBot KClosure)
		_			-> Nothing
		
	TData{}			-> makeOpTypeData tt
	TVar{}			-> Just $ TData KValue primTObj []
	TElaborate ee t		-> makeOpTypeT t
	_			-> freakout stage
					("makeOpTypeT: can't make operational type from " % show tt)
					Nothing
makeOpTypeT2 tt
 = case tt of
 	TForall v k t		-> makeOpTypeT2 t
	TFetters t fs		-> makeOpTypeT2 t
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
