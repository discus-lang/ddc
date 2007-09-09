
module Sea.Util
	( tossTops
	, eraseAnnotsTree 
	, typeIsUnboxed )
where

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var

import Sea.Exp
import Sea.Plate.Trans

-----------------------
-- 
eraseAnnotsTree pp
	= map (transformN (\n -> Nothing :: (Maybe ()))) pp


-----------------------
-- tossTops
--
tossTops ::	[Top a] -> Map Var [Top a]
tossTops ts
	= foldl tossTop' Map.empty ts


tossTop'	map p
 = Map.insertWith (++) (topName p) [p] map


topName :: Top a -> Var
topName	   xx
 = case xx of
	PSuper   v aa r ss	-> v
	PProto	 v aa r		-> v
	PCtor	 v aa r		-> v
	PHashDef s1 s2		-> Var.new "dummy"
	_			-> Var.new "dummy"



typeIsUnboxed
	:: Type -> Bool
	
typeIsUnboxed t
 = case t of 
 	TCon{}	-> True
	_	-> False
	
	
