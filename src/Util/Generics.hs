
module Util.Generics
	()

where
{-
	module Data.Generics,

	walkDown, walkDownM,
	walkUp,

	mkCollect, collect
)


where

import Data.Generics
import Control.Monad.State

-----
walkDown	:: GenericT -> GenericT
walkDown	= everywhere

-----
walkDownM 	:: Monad m
		=> GenericM m -> GenericM m
walkDownM	= everywhereM


-----
walkUp ::	GenericT -> GenericT
walkUp		= everywhere'



-----
-- Collect
-- 	For collecting various things from Data trees.
--
mkCollect :: (Typeable a, Typeable b, Typeable c)
	  => (b -> Maybe c) -> a -> State [c] a
	
mkCollect f x
 = case cast f of
 	Nothing	-> return x
	Just g
	 -> case g x of
	 	Nothing	-> return x
		Just c
		 -> do
		 	col	<- get
			put	$ c : col
	  
	  		return x
	  
collect	:: (forall b. Data b => b -> State [c] b)
	-> (forall a. Data a => a -> [c])
	
collect	f xx	= s'
 where
 	(_, s') = runState (walkDownM f xx) []
-}
