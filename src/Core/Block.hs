{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Ensure that the bodies of lambda abstractions, top level bindings and the right of
--	case alternatives are `XDo`s (block of statements). This makes the snippers job
--	easiser, becase all expressions that aren't in A-normal form are now enclosed
--	by a list of statements (which can be added to).
--
module Core.Block
	(blockGlob)
where
import Core.Exp
import Core.Glob
import Core.Plate.Trans


-- | Introduce XDo expressions into this tree.
blockGlob :: Glob -> Glob
blockGlob = mapBindsOfGlob blockP

blockP p
 = let 	table	= transTableId 
		{ transX	= \x -> return $ blockTreeX x 
		, transA	= \a -> return $ blockTreeA a 
		, transP	= \p -> return $ blockTreeP p 
		, transG	= \g -> return $ blockTreeG g }

   in	transZ table p


blockTreeX xx
 = case xx of
	XLAM v k x
	  -> XLAM v k (blockX_lambda x)

 	XLam v t x eff clo	
	  -> XLam v t (blockX_lambda x) eff clo

	_ -> xx


blockTreeA aa
 = case aa of
 	AAlt gs x	-> AAlt gs (blockX_tau x)


blockTreeG gg
 = case gg of
 	GExp w x	-> GExp w  (blockX x)


blockTreeP pp
 = case pp of
 	PBind v x	-> PBind v (blockX_lambda x)
	_		-> pp

-- | Force this expression to be an XDo
--	but allow XTau, XTet, XLam XLAM wrappers
blockX xx
 = case xx of
	XDo{}		-> xx
	XTau t x	-> blockX_lambda xx
	_		-> XDo [ SBind Nothing xx ]


-- | Force this expression to be an XDo
--	but allow XTau  wrappers.
blockX_tau xx
 = case xx of
 	XDo{}		-> xx
	XTau t x	-> XTau t $ blockX_tau x
	_		-> XDo [SBind Nothing xx]


blockX_lambda xx
 = case xx of
  	XLAM{}		-> xx
	XLam{}		-> xx
	XTau   t x	-> XTau t $ blockX_lambda x
	XDo{}		-> xx
	_		-> XDo [SBind Nothing xx]
