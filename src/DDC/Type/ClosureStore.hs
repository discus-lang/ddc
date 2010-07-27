
module DDC.Type.ClosureStore 
	(insert)
where

-- | An efficient data structure for managing sets of closure types.
data ClosureStore
	= ClosureStore
	{ csFree	:: Map Var (Set Var)
	, csVar		:: Set Var }
	deriving Show

insert :: Closure -> ClosureStore -> ClosureStore
insert clo cs
 = case clo of
	TVar k (UVar v)	-> cs { csVar = Map.insert v (csVar cs) }
	
		
