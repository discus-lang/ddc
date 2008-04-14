
module Type.Port
	( dropFMoresT
	, slurpContraClassVarsT)
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var

import Type.Exp
import Type.Class
import Type.Util
import Type.State
import Type.Plate.Collect
import Shared.Error
import Util

stage	= "Type.Port"

dropFMoresT :: Set Type -> Type -> SquidM Type
dropFMoresT tsContra tt
 = case tt of
 	TFetters fs t	
	 -> do	fs'	<- mapM (letifyFs tsContra) fs
	 	return	$ TFetters fs' t

	t -> return t
	
letifyFs tsContra ff
 = case ff of
 	FMore t1@(TClass k cid) t2
	 -> do	quantVars	<- gets stateQuantifiedVars
	 	v		<- makeClassName cid

		let result
			-- can't convert vars that have been quantified.
			| Map.member v quantVars 
			= ff
			
			-- can't convert vars that appear in contra-variant positions
			--	in the shape of the type
			| Set.member t1 tsContra
			= ff
			
			| otherwise
			= FLet t1 t2
			
		return result
			
	_ -> return ff




----
slurpContraClassVarsT :: Type -> [Type]
slurpContraClassVarsT tt
 = case tt of
	TForall vks t		-> slurpContraClassVarsT t
	TFetters fs t		-> slurpContraClassVarsT t
 	TFun t1 t2 eff clo	-> collectTClassVars t1 ++ slurpContraClassVarsT t2
	TData{}			-> []
	TApp{}			-> []
	TVar{}			-> []
	TClass{}		-> []
	TError{}		-> []	
	_			-> panic stage
				$ "slurpContraClassVarsT: no match for " % tt
