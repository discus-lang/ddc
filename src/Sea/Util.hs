
module Sea.Util
	( eraseAnnotsTree 
	, typeIsUnboxed )
where

import Sea.Exp
import Sea.Plate.Trans


eraseAnnotsTree pp
	= map (transformN (\n -> Nothing :: (Maybe ()))) pp

typeIsUnboxed :: Type -> Bool
typeIsUnboxed t
 = case t of 
 	TCon{}	-> True
	_	-> False
	
	
