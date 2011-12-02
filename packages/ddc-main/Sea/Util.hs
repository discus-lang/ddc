
-- | Utils for working on the Sea IR
module Sea.Util
	( eraseAnnotsTree 
	, eraseAnnots)
where
import Sea.Plate.Trans

-- | Erase all annotations in this list of things
eraseAnnotsTree pp
	= map (transformN (\n -> Nothing :: (Maybe ()))) pp

-- | Erase all annotations in this thing.
eraseAnnots p
	= (transformN (\n -> Nothing :: (Maybe ()))) p

