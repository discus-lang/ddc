
module Util.Data.Maybe
	( module Data.Maybe
	, maybeJust
	, liftMaybeR
	, makeMaybe
	, takeFirstJust
	, liftMaybe )
where

import Data.Maybe

-----
makeMaybe :: Bool -> a -> Maybe a
makeMaybe    test    val
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


liftMaybeR :: (a, Maybe b) -> Maybe (a, b)	 	
liftMaybeR    (a, mB) =
 case mB of 
   Nothing	-> Nothing
   Just	b	-> Just (a, b)


liftMaybe 
	:: Monad m
	=> (a -> m b) -> Maybe a -> m (Maybe b)

liftMaybe f mX
 = case mX of
 	Nothing	-> return Nothing
	Just x
	 -> do	x'	<- f x
	 	return	$ Just x'

