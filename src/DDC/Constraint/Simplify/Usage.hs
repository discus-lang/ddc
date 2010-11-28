{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Analyse how constrained variables are used.
module DDC.Constraint.Simplify.Usage
	( UseSide(..)
	, Usage(..))
where

data UseSide
	= OnLeft
	| OnRight
	deriving (Eq, Show)
	
-- | How a constrained type variable is used.
data Usage

	-- | Appears in an eq constraint.
	= UsedEq UseSide
	
	-- | Appears in a more-than constraint.
	| UsedMore UseSide
	
	-- | Appears in an inst constraint.
	| UsedInst UseSide

	-- | Appears in the binding vars list in a branch.
	| UsedBranchBind

	-- | Used in a projection constraint
	| UsedProject
	
	-- | Is generalised
	| UsedGen
	deriving (Eq, Show)
	



