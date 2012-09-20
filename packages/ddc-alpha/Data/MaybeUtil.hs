
-- | Bonus utils for working with `Maybe`s.
module Data.MaybeUtil
	( module Data.Maybe
	, makeMaybe
	, maybeJust
	, takeFirstJust
	, liftToMaybe
	, liftToMaybeSnd)
where
import Data.Maybe


-- | If True then Just the second parameter, else Nothing.
makeMaybe :: Bool -> a -> Maybe a
makeMaybe test val
	| test		= Just val
 	| otherwise	= Nothing


-- | If the value is a Just, return that value, otherwise inject the new Just.
maybeJust :: Maybe a -> a -> Maybe a
maybeJust m x
 = case m of
 	Just{}	-> m
	Nothing	-> Just x


-- | Take the first just element in this list
takeFirstJust :: [Maybe a] -> Maybe a
takeFirstJust	xx	
 = case catMaybes xx of
 	[]	-> Nothing
	(x:xs)	-> Just x


-- | Lift a monadic worker function into a Maybe.
liftToMaybe 
	:: Monad m
	=> (a -> m b) -> Maybe a -> m (Maybe b)

liftToMaybe f mX
 = case mX of
 	Nothing	-> return Nothing
	Just x
	 -> do	x'	<- f x
	 	return	$ Just x'


-- | Lift a monadic worker to the second element of a tuple,
--   using the provided value as the first element.
liftToMaybeSnd :: a -> Maybe b -> Maybe (a, b)	 	
liftToMaybeSnd a mB
 = case mB of 
   	Nothing	-> Nothing
   	Just	b	-> Just (a, b)



