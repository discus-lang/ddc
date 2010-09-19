
-- | Where's my shotgun?!?!...
module Util.Control.Monad
	( returnN
	, returnJ
	, partitionM
	, whenM
	, whenMaybeM
	, mLiftM
	, ttLiftM
	, t21LiftM
	, t22LiftM
	, liftM6)
where
import Control.Monad


-- | Shorthand for @return Nothing@
returnN :: Monad m => m (Maybe a)
returnN	  = return Nothing


-- | Shorthand for @return Just@
returnJ :: Monad m => a -> m (Maybe a)
returnJ x = return $ Just x


-- | Partition a list using a monadic computation.
partitionM :: Monad m =>  (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f []	= return ([], [])
partitionM f (x:xs)
 = do 	r		<- f x
	(aa, bb)	<- partitionM f xs
	if r 
	 then return (x : aa, 	bb)
	 else return (aa,   	x : bb)		


-- | Like `when`, but with teh monadz.
whenM :: Monad m => m Bool -> m () -> m ()
whenM    f op
 = do 	b	<- f
	when b op


-- | like `maybe`, but with teh monadz.
whenMaybeM 
	:: Monad m 
	=> Maybe a -> m b -> (a -> m b) -> m b
whenMaybeM mm def f
 = case mm of
	Nothing	-> def
	Just x	-> f x


-- | Apply a computation to the element in a `Maybe`
mLiftM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mLiftM f m
 = case m of
	Just x	-> f x >>= \x' -> return (Just x')
	Nothing	-> return Nothing


-- | Apply monaic computations to both elements of a tuple.
ttLiftM :: Monad m => (a -> m b) -> (c -> m d) -> (a, c) -> m (b, d)
ttLiftM f g (x, y)
 = do	x'	<- f x
	y'	<- g y
	return	(x', y')


-- | Apply a computation to the first element of a pair.
t21LiftM :: Monad m => (a -> m c) -> (a, b) -> m (c, b)
t21LiftM f (x, y)
 = do	x'	<- f x
	return	(x', y)


-- | Apply a computation to the second element of a pair.
t22LiftM :: Monad m => (b -> m c) -> (a, b) -> m (a, c)
t22LiftM f (x, y)
 = do	y'	<- f y
	return	(x, y')


-- | Promote a function to a monad, scanning the monadic arguments from
--   left to right (cf. 'liftM2').
liftM6  :: Monad m 
	=> (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) 
	-> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r

liftM6 f m1 m2 m3 m4 m5 m6
	= do { 	x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6;
		return (f x1 x2 x3 x4 x5 x6) }


