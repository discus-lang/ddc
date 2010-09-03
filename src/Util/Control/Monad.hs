
module Util.Control.Monad
	( returnN, returnJ
	, partitionM
	, whenM )
where
import Control.Monad

returnN :: Monad m => m (Maybe a)
returnN		= return Nothing


returnJ :: Monad m => a -> m (Maybe a)
returnJ x 	= return $ Just x


partitionM :: Monad m =>  (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f []	= return ([], [])
partitionM f (x:xs)
 = do 	r		<- f x
	(aa, bb)	<- partitionM f xs
	if r 
	 then return (x : aa, 	bb)
	 else return (aa,   	x : bb)		


whenM :: Monad m => m Bool -> m () -> m ()
whenM    f op
 = do
 	b	<- f
	when b op

