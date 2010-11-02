{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Simplify.Boxing
	( RuleBoxing(..)
	, simplifyUnboxBoxX)
where
import DDC.Core.Exp

data RuleBoxing
	= RuleBoxingUnboxBox
	| RuleBoxingUnboxForceBox
	deriving (Eq, Ord, Show)

-- | Zap out unbox/box and unbox/force/box primops.
simplifyUnboxBoxX 
	:: Monad m
	=> (RuleBoxing -> m ()) -> Exp -> m Exp

simplifyUnboxBoxX count xx
 = case xx of 

	-- unbox/box
 	XPrim MUnbox [r1, XPrim MBox [r2, x]]
		| r1 == r2	
		-> do	count RuleBoxingUnboxBox
			return x
		
	-- unbox/force/box
	XPrim MUnbox [r1, XPrim MForce [XPrim MBox [r2, x]]]
		| r1 == r2
		-> do	count RuleBoxingUnboxForceBox
			return x
		
	_	-> return xx
