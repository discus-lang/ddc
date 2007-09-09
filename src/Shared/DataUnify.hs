
module Shared.DataUnify
(
	module Data.Typeable
	
)

where

-----
import Control.Monad.State
import Data.Typeable


----------------------
class Typeable a => DataUnify a where

	-- 
	gUnify 	:: (forall b. DataUnify b => b -> b -> b) 
		-> a -> a -> a

	gUnify f a b	= evalState (gUnifyM (\x y -> return $ f x y) a b) 0
 
	--
 	gUnifyM	:: Monad m
		=> (forall b. DataUnify b => b -> b -> m b)
		-> a -> a -> m a
	
	gUnifyM f a	= error "gUnifyM: not defined"


-----------------------
-- Make Functions
--
make	:: (Typeable a, Typeable b)
	=> (b -> b -> b) 
	-> a -> a -> a

make f
 = case cast f of
	Just g	-> g
	Nothing	-> (\a b -> a)


--
makeM	:: (Typeable a, Typeable b,
	    Typeable (m a), Typeable (m b),
	    Monad m)
		    
	=> (b -> b -> m b)
	->  a -> a -> m a
		
makeM f
 = case cast f of
 	Just g	-> g
	Nothing -> (\a b -> return a)


{-
-----------------------
-- Walk Functions
--

walkUp		:: DataUnify a
		=> (forall b. DataUnify b => b -> b -> b)
		-> a -> a -> a
		
walkUp f x			

-}









