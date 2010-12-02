{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module Data.Hashable
	(Hashable(..))
where
import Data.Int

class Hashable a where
	hash :: a -> Int32
	
