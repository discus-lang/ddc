
-- | Utils for working on the Sea IR
module Sea.Util
	( eraseAnnotsTree 
	, eraseAnnots
	, typeIsUnboxed )
where

import Sea.Exp
import Sea.Plate.Trans

-- | Erase all annotations in this list of things
eraseAnnotsTree pp
	= map (transformN (\n -> Nothing :: (Maybe ()))) pp

-- | Erase all annotations in this thing.
eraseAnnots p
	= (transformN (\n -> Nothing :: (Maybe ()))) p

-- | Check if a type represents an unboxed value.
typeIsUnboxed :: Type -> Bool
typeIsUnboxed t
 = case t of 
 	TCon{}	-> True
	_	-> False
	