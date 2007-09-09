
module Sea.Proto
(
	addSuperProtosTree

)

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import Sea.Exp
import Sea.Util


addSuperProtosTree :: 	Tree () -> Tree ()
addSuperProtosTree	tree
 = concat
 	$ map (\p -> case p of
			PSuper v args r ss	
			 -> [ PProto v (map snd args) r
			    , p]
			    
			_ -> [p])
	$ tree

