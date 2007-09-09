
module Type.Context
(
	reduceContextT
)

where

import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import Type.Exp
import Type.Plate

reduceContextT 
	:: Map Var Fetter
	-> Type -> Type
	
reduceContextT classInst tt
	= transformT (reduceContextT' classInst) tt
	
reduceContextT' classInst tt
 = case tt of
 	TFetters fs t	
	 -> let	fs'	= catMaybes
	 		$ map (reduceContextF classInst) fs
	    in	case fs' of
	    		[]	-> t
			_	-> TFetters fs' t
			
	_ 		-> tt
	
reduceContextF classInst ff
 	| FConstraint v ts	<- ff
	, Just f		<- Map.lookup v classInst
	= Nothing
	
	| otherwise
	= Just ff 	
