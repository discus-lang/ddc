
module Sea.Util
	( eraseAnnotsTree 
	, eraseAnnots
	, typeIsUnboxed )
where

import Sea.Exp
import Sea.Plate.Trans

eraseAnnotsTree pp
	= map (transformN (\n -> Nothing :: (Maybe ()))) pp

eraseAnnots p
	= (transformN (\n -> Nothing :: (Maybe ()))) p

typeIsUnboxed :: Type -> Bool
typeIsUnboxed t
 = case t of 
 	TCon{}	-> True
	_	-> False
	
	
