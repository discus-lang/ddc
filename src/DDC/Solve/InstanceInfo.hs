{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Records information aboud how the type scheme of a bound variable was instantiated.
module DDC.Solve.InstanceInfo
	(InstanceInfo(..))
where
import DDC.Var
import DDC.Type.Exp

-- An `InstanceInfo` carries information about how the type scheme of 
--	a bound variable was instantiated, from the solver to the desugarer. 
--	This allows it to fill in type parameters when converting to the Core IR.
--
data InstanceInfo param

	-- | An occurrence of a bound variable.
	= InstanceLambda
		{ instBoundVar		:: Var		-- ^ The bound occurrence.
		, instBindingVar	:: Var		-- ^ The binding occurrence.
		, instMaybeType		:: (Maybe Type)	-- ^ Type of the bound varaible.
		}
	
	-- | A non-recursive occurrence of a let-bound variable.
	| InstanceLet
		{ instBoundVar		:: Var		-- ^ The bound occurrence.
		, instBindingVar	:: Var		-- ^ The binding occurrence.
		, instParameters	:: [param]	-- ^ Instance types corresponding to each of the type
							--	variables in the outermost forall of the type scheme.
		, instType		:: Type		-- ^ The type scheme that was instantiated.
		}
	
	-- | A recursive occurrence of a let-bound variable.
	| InstanceLetRec
		{ intBoundVar		:: Var		-- ^ The bound occurrence.
		, intBindingVar		:: Var		-- ^ The binding occurrence.
		, intMaybeType		:: Maybe Type 	-- ^ The type scheme that was instantiated.
		}

	deriving Show
