
-- | Erases simple assignments of the form, v1 = v2 and substitutes them
--   into the subsequent code.
--
--   We can erase a v1 = v2 assignment if v1 is only assigned to once.
--   In this case v1 is just acting as an alias for the value on the RHS.
--
--   However, if v1 appears on the LHS of more than one assignment statement, 
--   such as with:
--	
--     switch (exp) {
--         case c1: v1 = v3; break;
--         case c2: v1 = v4; break;
--     }
--
--   .. then v1 is acting as a communication channel and these assignments
--   can not be erased.
--
--
module Sea.Sub
	(subTree)
where
import Util
import Sea.Plate.Trans
import DDC.Sea.Exp
import DDC.Sea.Pretty		()
import DDC.Main.Error
import qualified Data.Map	as Map

stage	= "Sea.Sub"

type MapAssignCount	= Map Var Int
type MapAssignVar	= Map Name Name


-- | Erase simple assignments in this tree
subTree	:: Tree ()	-> Tree ()
subTree	ps
 =	map subTreeP ps


-- | Erase simple assignments in this top level thing
subTreeP :: Top () -> Top ()
subTreeP p
 = case p of
 	PSuper v aa t ss
	 -> let
		-- walk through the tree and count how many times each variable is assigned to
	 	mapAssignCount	= execState (mapM (transformSM assignCountS) ss) 
				$ Map.empty

		-- use the information from the previous pass to substitute for variables
		--	that are only assigned to once.
		(pErased, mapAssignVar)
				= runState ( transformSSM (eraseTreeSS mapAssignCount) p)
				$ Map.empty

		-- Substitute variable aliases back into super body.
		tableSub	= transTableModV mapAssignVar
		pSubed		= transZ tableSub pErased

	    in pSubed

	_ -> p

-----
transTableModV varMap
	= (transTableId return)
		{ transX	= \x -> return $ sinkVarX varMap x }


sinkVarX :: MapAssignVar -> Exp () -> Exp ()
sinkVarX varMap xx
	| XVar name t	<- xx
	, Just name'	<- Map.lookup name varMap
	= sinkVarX varMap (XVar name' t)
	
	| otherwise
	= xx
	


-- | If this statement is an assignment, then remember that we've
--	assigned to the variable on the left.
assignCountS 
	:: Stmt () 
	-> State MapAssignCount (Stmt ())

assignCountS s
 = case s of
 	SAssign (XVar (NAuto v) _) _ _
	 -> do	modify $ accMap 1 (\x -> x + 1) v
	 	return s
	 
	_ -> 	return s
		
accMap init modFun key m 
 = case Map.lookup key m of
 	Nothing	-> Map.insert key init		m
	Just x	-> Map.insert key (modFun x)	m
 	

-----
eraseTreeSS 
	:: MapAssignCount 
	-> [Stmt ()] 
	-> State MapAssignVar [Stmt ()]

eraseTreeSS assignCount xx
 = case xx of
 	[]	-> return xx
	
 	s@(SAssign (XVar n1 _) _ (XVar n2 _)) : ss
	 -> case Map.lookup (varOfName n1) assignCount of

	 	Nothing 	
		 -> panic stage 
		 $  "subTreeSS: assigned var " % n1 % " has no assignCount entry\n"

		Just count
		 |  count == 1	
		 -> do 	modify (Map.insert n1 n2)
			eraseTreeSS assignCount ss

		 | otherwise
		 -> do 	ss'	<- eraseTreeSS assignCount ss
			return	$ s : ss'

	s : ss
	 -> do	ss'		<- eraseTreeSS assignCount ss
	 	return $ s : ss'

