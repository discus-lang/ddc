{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | An interface to the common operations supported by all container types.
module DDC.Util.Container
	(Container (..))
where
import Data.Map			(Map)
import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-- | Common operations supported by all container types.
class Container (c :: * -> *) where

	-- | Check if a container contains no elements.
	isEmpty 	:: c a -> Bool
	
	-- | Get the elements of a container as a list. 
	--	For `Map`-like containers, this discards the key value and only
	--	returns the element.
	elemsList	:: c a -> [a]

	
instance Container [] where
	isEmpty		= null
	elemsList	= id

	
instance Container Set where
	isEmpty		= Set.null
	elemsList	= Set.toList

	
instance Ord k => Container (Map k) where
	isEmpty		= Map.null
	elemsList	= Map.elems