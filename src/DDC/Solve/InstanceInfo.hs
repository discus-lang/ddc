
-- | Records information aboud how the type scheme of a bound variable was instantiated.
module DDC.Solve.InstanceInfo
	(InstanceInfo(..))
where
import DDC.Var
import Type.Exp

-- An `InstanceInfo` carries information about how the type scheme of 
--	a bound variable was instantiated from the solver to the desugarer, 
--	so it can fill in type parameters when converting to the Core IR.
--
data InstanceInfo param

	-- | An occurrence of a bound variable.
	= InstanceLambda
		Var 		-- ^ The bound occurrence.
		Var		-- ^ The binding occurrence.
		(Maybe Type)	-- ^ Type of the bound varaible.
	
	-- | A non-recursive occurrence of a let-bound variable.
	| InstanceLet
		Var		-- ^ The bound occurrence.
		Var		-- ^ The binding occurrence.
		[param]		-- ^ Instance types corresponding to each of the type
				--	variables in the outermost forall of the type scheme.
		Type		-- ^ The type scheme that was instantiated.
		
	-- | A recursive occurrence of a let-bound variable.
	| InstanceLetRec
		Var		-- ^ The bound occurrence.
		Var		-- ^ The binding occurrence.
		(Maybe Type)	-- ^ The type scheme that was instantiated.

	deriving Show
