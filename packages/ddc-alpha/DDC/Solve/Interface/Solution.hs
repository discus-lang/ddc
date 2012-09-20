{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Solve.Interface.Solution
	( Solution	(..)
	, InstanceInfo	(..))
where
import DDC.Type.Exp
import DDC.Var
import Data.Map		(Map)

-- | Solution of type inferencer.
data Solution
	= Solution
	{ -- | Canonical names for given variables.
	  solutionCanon			:: Map Var Var

	  -- | Types for given variables.
	, solutionTypes			:: Map Var Type
	
	  -- | How the type of each occurrence of a let-bound variable was instantiated.
	, solutionInstanceInfo		:: Map Var (InstanceInfo Type)

	  -- | The constraints on each region variable.
	, solutionRegionClasses		:: Map Var [Var] 
	
	  -- | How projections were resolved
	, solutionProjResolution	:: Map Var Var }


-- An `InstanceInfo` carries information about how the type scheme of 
--	a bound variable was instantiated, from the solver to the desugarer. 
--	This allows it to fill in type parameters when converting to the Core IR.
--
data InstanceInfo param

	-- | An occurrence of a bound variable.
	= InstanceLambda
		{ -- | The bound occurrence
		  instBoundVar		:: Var

		  -- | The binding occurrence
		, instBindingVar	:: Var

		  -- | Type of the variable.
		, instMaybeType		:: Maybe Type
		}
	
	-- | A non-recursive occurrence of a let-bound variable.
	| InstanceLet
		{ -- | The bound occurrence.
		  instBoundVar		:: Var

		  -- | The binding occurrence.
		, instBindingVar	:: Var

		  -- | Instance types for each of the type vars in the outermost
		  --   forall of the scheme.
		, instParameters	:: [param]
		
		  -- | Type scheme that was instantiated.
		, instType		:: Type
		}
	
	-- | A recursive occurrence of a let-bound variable.
	| InstanceLetRec
		{ 
		  -- | The bound occurrence.	
		  intBoundVar		:: Var

		  -- | The binding occurrence.
		, intBindingVar		:: Var

		  -- | Type scheme that was instantiated.
		, intMaybeType		:: Maybe Type
		}

	deriving Show
