module Util
	( module Control.Monad.State.Strict
	, module Data.TupleUtil
	, module Data.ListUtil
	, module Data.MaybeUtil
	, module Util.Control.Monad
	, module Util.Tunnel
	, module Util.Pretty
	, test_Util
	, Map
	, Set
	, (=@=)
	, orf)
where
import Control.Monad.State.Strict
import Data.TupleUtil
import Data.ListUtil
import Data.MaybeUtil
import Data.WorkGraph
import Util.Control.Monad
import Util.Tunnel
import Util.Pretty

import Data.Map		(Map)
import Data.Set		(Set)
import GHC.Base


-- | Compare tags of constructors in these two values
(=@=) :: a -> a -> Bool
(=@=) a b = getTag a ==# getTag b

orf :: 	a -> [a -> Bool] -> Bool
orf	x fs
 = 	foldl (\a f -> a || f x) False fs
	


test_Util
	=  test_UtilDataList
	++ test_UtilDataWorkGraph

