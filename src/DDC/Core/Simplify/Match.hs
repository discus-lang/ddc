{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Simplify.Match
	( RuleMatch (..)
	, simplifyMatchX)
where
import DDC.Core.Exp

data RuleMatch
	= RuleMatchNoAlts
	deriving (Eq, Ord, Show)

-- | Simpify match expressions.
simplifyMatchX 
	:: Monad m 
	=> (RuleMatch -> m ()) -> Exp -> m Exp

simplifyMatchX count xx
 = case xx of
	
	-- | Dump matches with no alternatives
	XMatch [AAlt [] x]	
	 -> do	count RuleMatchNoAlts
	 	return x
	
	XDo [SBind Nothing x]
	 -> do	return x
	
	_ -> return xx
