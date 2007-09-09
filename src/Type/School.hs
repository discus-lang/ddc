
module Type.School
(
{-	allocClass,
	addToClass,
	lookupClass,
	updateClass,
	addClassIdSubs,
	sinkClassId,
	lookupVarToClassId,
	makeClassV,
	addBackRef,
	mergeClasses
-}
)

where

import Util

import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Array.IO	as Array
import Data.Array.IO		(IOArray, writeArray, readArray)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Util.Data.Bag	as Bag
import Util.Data.Bag		(Bag)

import Shared.Var		(Var)
import Shared.Error		

import Type.Exp
import Type.Util
import Type.Base
import Type.State

-----
stage	= "Type.Squid.School"


		

		
	  
	  



