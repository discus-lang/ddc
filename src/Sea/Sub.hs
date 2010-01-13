-----
-- Sea.Sub
--	Erases simple assignments of the form, v1 = v2 
--		and substitutes them into the subsequent code.
--
--	We can erase a v1 = v2 assignment if v1 is only ever assigned to
--	a single time. In this case v1 is acting as a simple alias for the
--	value on the RHS.
--
--	If the variable v1 appears on the LHS of more than one assignment 
--	statement, eg:
--	
--		switch (exp) {
--		 case c1:	v1 = v3; break;
--		 case c2:	v1 = v4; break;
--		}
--
--	.. then v1 is acting as a communication channel and these assignments
--	can not be erased.
--
--
module Sea.Sub
	(subTree)

where

import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import Shared.Error

import Sea.Exp
import Sea.Plate.Trans

import qualified Debug.Trace	as Debug

-----
stage	= "Sea.Sub"

-----
type MapAssignCount	= Map Var Int
type MapAssignVar	= Map Var Var


-----
subTree	::	Tree ()	-> Tree ()
subTree		ps
 =	map subTreeP ps

-----	
subTreeP ::	Top ()	-> Top ()
subTreeP	p
 = case p of
 	PSuper v aa t ss
	 -> let
	 	mapAssignCount	= execState (mapM (transformSM assignCountS) ss) 
				$ Map.empty

		-- Erase v1 = v2 statements, so long as the assign count for v1 is 1.
		--	Also collect a map of what assignments were erased.
		--
		(pErased, mapAssignVar)
				= runState ( transformSSM (eraseTreeSS mapAssignCount) p)
				$ Map.empty

		-- Substitute variable aliases back into super.
		--
		tableSub	= transTableModV mapAssignVar
		pSubed		= transZ tableSub pErased

	    in pSubed

	_ -> p

-----
transTableModV varMap
	= (transTableId return)
		{ transV	= sinkVar varMap }


sinkVar varMap v
 = case Map.lookup v varMap of
 	Nothing	-> return v
	Just v'	-> sinkVar varMap v'

-----
assignCountS ::	Stmt () -> State MapAssignCount (Stmt ())
assignCountS	s
 = case s of
 	SAssign (XVar v _) _ _
	 -> do	modify $ accMap 1 (\x -> x + 1) v
	 	return s
	 
	_ -> 	return s
	
	
accMap init modFun key m 
 = case Map.lookup key m of
 	Nothing	-> Map.insert key init		m
	Just x	-> Map.insert key (modFun x)	m
 	

-----
eraseTreeSS ::	MapAssignCount -> [Stmt ()] -> State MapAssignVar [Stmt ()]

eraseTreeSS assignCount xx
 = case xx of
 	[]	-> return xx
	
 	s@(SAssign (XVar v1 _) _ (XVar v2 _)) : ss
	 -> case Map.lookup v1 assignCount of

	 	Nothing 	
		 -> panic stage 
		 $  "subTreeSS: assigned var " % v1 % " has no assignCount entry\n"

		Just count
		 |  count == 1	
		 -> do
		 	modify (Map.insert v1 v2)
			eraseTreeSS assignCount ss

		 | otherwise
		 -> do
		 	ss'	<- eraseTreeSS assignCount ss
			return	$ s : ss'

	s : ss
	 -> do
		ss'		<- eraseTreeSS assignCount ss
	 	return $ s : ss'

