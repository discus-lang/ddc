{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Simplify.Boxing
	( RuleBoxing(..)
	, simplifyUnboxBoxX)
where
import DDC.Core.Exp
import Core.Util

data RuleBoxing
	= RuleBoxingUnboxBox
	| RuleBoxingUnboxForceBox
	deriving (Eq, Ord, Show)

-- | Zap out unbox/box and unbox/force/box primops.
simplifyUnboxBoxX 
	:: Monad m
	=> (RuleBoxing -> m ()) -> Exp -> m Exp

simplifyUnboxBoxX count xx

	-- unbox/box
	| [Left (XPrim MUnbox{} _), Right r1, Left x1]	<- flattenApps xx
	, [Left (XPrim MBox{}   _), Right r2, Left x2]	<- flattenApps x1
	, r1 == r2
	= do	count RuleBoxingUnboxBox
		return x2
				
	-- unbox/force/box
	| [Left (XPrim MUnbox{} _),  Right r1, Left x1]	<- flattenApps xx
	, [Left (XPrim MForce{} _),  Left x2]		<- flattenApps x1
	, [Left (XPrim MBox{}   _),  Right r2, Left x3]	<- flattenApps x2
	, r1 == r2
	= do	count RuleBoxingUnboxForceBox
		return x3
	
	| otherwise
	= return xx
