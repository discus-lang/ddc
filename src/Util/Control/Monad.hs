
module Util.Control.Monad
	( returnN, returnJ
	, mapMuz
	, mapMt2_1, mapMt2_2
	, partitionM
	, whenM )

where

import Control.Monad

returnN :: Monad m => m (Maybe a)
returnN		= return Nothing

returnJ :: Monad m => a -> m (Maybe a)
returnJ x 	= return $ Just x

-----
-- mapMuz
--
--	Monadic map and unzip, rolled into one function
--
mapMuz  :: Monad m 
	=> (a -> m (b, c))  -> [a]
	-> m ([b], [c])

mapMuz 	f aa
 = do
 	bcs	<- mapM f aa
	return	 $ unzip bcs


-----
mapMt2_1 :: Monad m => (a -> m c) -> [(a, b)] -> m [(c, b)]

mapMt2_1   f [] 		
	= return []

mapMt2_1   f ((a, b):xs)	
 = do
 	c	<- f a
	rest	<- mapMt2_1 f xs
	return	$ (c, b) : rest

-----
mapMt2_2 :: Monad m => (b -> m c) -> [(a, b)] -> m [(a, c)]

mapMt2_2   f [] 		
	= return []

mapMt2_2   f ((a, b):xs)	
 = do
 	c	<- f b
	rest	<- mapMt2_2 f xs
	return	$ (a, c) : rest

{-
liftFieldModifier 	:: ( state -> field
	   		   , field -> state -> state )
			-> (a -> field -> (b, field))
			-> a -> State state b
	   
liftFieldModifier 	(getF, setF) func a
 = do
 	field		<- gets getF
	let (b, field')	=  func a field
	
	modify (\s -> setF field' s)
	return b
-}	

-----
partitionM :: Monad m =>  (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f []
	= return ([], [])

partitionM f (x:xs)
 = do
 	r		<- f x
	(aa, bb)	<- partitionM f xs

	if r 
		then return (x : aa, 	bb)
		else return (aa,   	x : bb)		


-----
whenM :: Monad m => m Bool -> m () -> m ()
whenM    f op
 = do
 	b	<- f
	when b op

