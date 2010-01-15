
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
	TConstrain tBody crs@(Constraints crsEq crsMore crsOther)
	 -> do	(crsEq', mcrsMore)	
				<- mapAccumLM (letifyFs tsContra) crsEq 
				$  Map.toList crsMore

		let crsMore'	= Map.fromList $ catMaybes mcrsMore
		let crs' 	= Constraints crsEq' crsMore' crsOther
		return	$ addConstraints crs' tBody

	t -> return t

letifyFs
	:: Set Type				-- ^ parameter variables
	-> Map Type Type			-- ^ equality constraint set
	-> (Type, Type)				-- ^ more than constraint
	-> SquidM ( Map Type Type		-- new equality constraint set
	   	  , Maybe (Type, Type)	)	-- new more constraint (if any)

letifyFs tsContra fsEq fMore
 = case fMore of
 	(t1@(TClass k cid), t2)
	 -> do	quantVars	<- gets stateQuantifiedVars
	 	v		<- makeClassName cid

		let result
			-- can't convert vars that have been quantified.
			| Map.member v quantVars 
			= (fsEq, Just fMore)
			
			-- can't convert vars that appear in contra-variant positions
			--	in the shape of the type
			| Set.member t1 tsContra
			= (fsEq, Just fMore)
			
			| otherwise
			= (Map.insert t1 t2 fsEq, Nothing)
			
		return result
			
	_ -> return (fsEq, Just fMore)


----
slurpContraClassVarsT :: Type -> [Type]
slurpContraClassVarsT tt
 = case tt of
	TForall b k t	-> slurpContraClassVarsT t
	TFetters t fs	-> slurpContraClassVarsT t
	TConstrain t crs -> slurpContraClassVarsT t

	TApp t1 t2
	  | Just (t11, t12, eff, clo)	<- takeTFun tt
	  -> (Set.toList $ collectTClassVars t11) ++ slurpContraClassVarsT t12
	
	  | Just _	<- takeTData tt
	  -> []
	
	  | otherwise
	  -> slurpContraClassVarsT t1 ++ slurpContraClassVarsT t2
	
	TVar{}		-> []
	TCon{}		-> []
	TClass{}	-> []
	TError{}	-> []	
	_		-> panic stage
			$ "slurpContraClassVarsT: no match for " % tt

