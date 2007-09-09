
module Type.Closure.Trim
	( trimClosureT )
	
where

import Util
import Shared.Error
import Type.Exp
import Type.Plate
import Type.Util
import Type.Pretty

-----
stage	= "Type.Closure.Trim"

-----
-- trimClosureF
--
trimClosureT :: Type -> Type
trimClosureT tt		
 = case tt of
 	TFetters fs t	-> TFetters (map trimClosureF fs) t
	_		-> tt
	

trimClosureF ::	Fetter	-> Fetter
trimClosureF ff
 = case ff of
 	FLet c1 c2	
	 |  kindOfType c2 == KClosure
	 -> FLet c1 $ makeTSum KClosure $ trimClosureC [] c2

	_		-> ff




trimClosureBoundF :: [(Var, Kind)] -> Fetter -> [Closure]
trimClosureBoundF qs ff
 = case ff of
 	FLet t1 t2
	 |  kindOfType t2 == KClosure
	 -> trimClosureC qs t2

	_		-> []


trimClosureC :: [(Var, Kind)] -> Closure -> [Closure]
trimClosureC    qs cc
 = case cc of
 	TVar   {}		-> [cc]
	TClass {}		-> [cc]
	TBot  KClosure		-> []
	TTop  KClosure 		-> [cc]

	TSum  KClosure cs	-> catMap (trimClosureC qs) cs

	TMask KClosure t1 t2	
	 -> let	t1'	= makeTSum KClosure $ trimClosureC qs t1
	    in	[TMask KClosure t1' t2]

	TTag   v		-> [cc]

	TFree v t
	 |  collectClassIds t == []
	 -> []

	 | trimKeepT t	
 	 -> let	(t', fs)	= stripFettersT t
	 	
		vsFree		= nub $ freeVarsT t
		qsQuant		= filter (\(v, t) -> elem v vsFree) qs
		
		t2		= addTForallVKs qsQuant t
		
	    in	case collectClassIds t2 of
		 []	-> 			catMap (trimClosureBoundF qs) fs
		 _	-> TFree v t2 	: 	catMap (trimClosureBoundF qs) fs

	 | otherwise	
	 -> [TFree v (makeTSum KClosure $ trimClosureQuantT qs t)]
	 
	TFetters fs c	-> catMap (trimClosureBoundF qs) fs ++ trimClosureC qs c
	 
	_ -> panic stage
		$ "trimClosureC: no match for " % show cc


trimClosureQuantT :: [(Var, Kind)] -> Type -> [Closure]
trimClosureQuantT	qs tt
 = case tt of
 	TForall vks t		-> trimClosureQuantT (vks ++ qs) t
	TFetters fs t		-> catMap (trimClosureBoundF qs) fs
	TFun t1 t2 eff clo	-> trimClosureC qs clo
	_			-> []


trimKeepT :: Type -> Bool
trimKeepT tt
 = case tt of
	TClass{}	-> True
	TVar{}		-> True
	TFun{}		-> False
	TData{}		-> True
	TUnify{}	-> True
	TMask{}		-> True
	TFree{}		-> True
	TAccept t	-> trimKeepT t

 	TForall vks t	-> trimKeepT t
	TFetters fs t	-> trimKeepT t

	_		-> panic stage $ "trimKeepT: no match for " % tt


{-	 let	t'	= packTypeLs ls $ loadType ls t
	    in  case collectClassIds t' of
	          []	-> TEmpty
		  _	-> TFree v t'
-}	    

