
module DDC.Desugar.ToCore.Clean
--	(cleanTree)
where
-- import DDC.Core.Glob
-- import DDC.Core.Exp

-- stage = "DDC.Desguar.ToCore.Clean"
{-

-- | With higher order programs with complex more-than constraints we sometimes get
--   constraints on variables that aren't bound by a type-lambda.
--   For example:
--
--  @
--   main = \_ : Unit -> ... show (f (Int %r1 -(!e1 :> Read %r1)> Int %r2)) ...
--  @
--
--  That !e1 isn't bound anywhere, so we might as well instantiate it to its constraint.
--  
cleanGlob :: Glob -> Glob
cleanGlob = mapBindsOfGlob cleanTop

cleanTop  :: Top -> Top
cleanTop (PBind v xx) 	= PBind v (cleanExp xx)
cleanTop _		= panic stage $ "cleanTop: no match"

cleanX :: Set.Var	-- ^ Type variables bound in the environment.
	 -> Exp -> Exp

cleanX env xx
 = case xx of
	XVar v t	-> XVar v (cleanT env t)
	XLit{}		-> xx
	XLAM b k x	
	 -> let env'	= Set.union env (Set.fromList $ maybeToList $ takeVarOfBind b)
	    in  XLam (cleanB env' b) (cleanK env' k) (cleanX env' x)
	XAPP x t	-> XAPP (cleanX x) (cleanT t)

	XLam v t x e c	-> XLam v (cleanT t) x (cleanT e) (cleanT c)
	XApp x1 x2	-> XApp (cleanX x1) (cleanX x2)
	XDo    ss	-> XDo  (map cleanS ss)
	XMatch alts	-> XMatch 


-- | Cleaner monad holds an environment of bound type variables.
type CleanM a	= State (Set Var) a

cleanX xx
	= 
	
cleanX_enter xx
 = case xx of
	XLAM{}
	 -> do	modify $ \env -> Set.union env (Set.fromList $ maybeToList $ takeVarOfBind b)
		return xx

	_ -> return xx
-}	
