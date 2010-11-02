{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Simplify.Match
	( RuleMatch (..)
	, simplifyMatchX
	, simplifyMatchA)
where
import DDC.Core.Exp

data RuleMatch
	= RuleMatchNoAlts
	| RuleMatchInMatch
	| RuleSingleMatchInAlt
	deriving (Eq, Ord, Show)

-- | Simpify match expressions.
simplifyMatchX 
	:: Monad m 
	=> (RuleMatch -> m ()) 
	-> Bool			-- ^ Whether to eat up XTay nodes.
	-> Exp -> m Exp

simplifyMatchX count eatXTaus xx
 = case xx of
	-- By the time the simplifier runs we don't need XTau nodes.
	-- We should really annotate top level bindings with their types
	-- and eradicate XTaus entirely.
	XTau _ x
	 | eatXTaus	-> return x

	-- Eat up boring XDo expressions.
	--   do { x } ==> x
	XDo [SBind Nothing x]
	 -> do	return x
	
	-- Dump matches with no alternatives.
	--   match { | otherwise = x } ==> x
	XMatch [AAlt [] x]	
	 -> do	count RuleMatchNoAlts
	 	return x

	-- Combine nested match expresions where the outer one only has a single alternative.
	--    match { | ..gs1..      = match { | ..gs2.. = x } }
	-- => match { | ..gs1 gs2..  = x }
	XMatch  [AAlt gs1 (XMatch [AAlt gs2 x])]
	 -> do	count RuleMatchInMatch
		return $ XMatch [AAlt (gs1 ++ gs2) x]
	
	_ -> return xx


-- | Simplify alternatives
simplifyMatchA 
	:: Monad m
	=> (RuleMatch -> m ())
	-> Alt -> m Alt
	
simplifyMatchA count aa
 = case aa of

	-- Combine matches with a single alternative into enclosing alternatives.
	-- alt   | ..gs1..      = match { gs2 = x }
	-- =>    | ..gs1 gs2 .. = x
	AAlt gs1 (XMatch [AAlt gs2 x])
	 -> do	count RuleSingleMatchInAlt
		return	$ AAlt (gs1 ++ gs2) x

	_ -> return aa

