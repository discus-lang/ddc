{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module Data.Hashable
	( Hashable(..)
	, Hash.hashInt)
where
import Data.Int
import qualified Data.HashTable	as Hash

class Hashable a where
	hash :: a -> Int32

instance Hashable Int where
	hash = Hash.hashInt
	
