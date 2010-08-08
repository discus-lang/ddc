{-# OPTIONS -fwarn-incomplete-patterns #-}

module Desugar.Plate.FreeVars
	(freeVars)
where
import Desugar.Exp
import Desugar.Util
import DDC.Var
import DDC.Util.FreeVars
import Data.Set			((\\), unions, empty, singleton)
import Util			hiding ((\\))
import qualified Data.Set 	as Set

 
-- Exp ---------------------------------------------------------------------------------------------
instance FreeVars (Exp a) where
 freeVars xx
  = case xx of
	XNil				-> empty

	XVoid{}				-> empty

	XLit{}				-> empty

	XVar 	n v			-> singleton v

	XProj   _ x j			-> freeVars x

	XProjT  _ t j			-> freeVars t

	XLambda _ v x			-> freeVars x \\ singleton v

	XApp _ x1 x2			-> unions [freeVars x1, freeVars x2]

	XMatch _ Nothing as		-> unions (map freeVars as)
	XMatch _ (Just x) as		-> unions (freeVars x : map freeVars as)

	XDo _ ss		
 	 -> let	freePerStmt	= Set.unions (map freeVars ss)
		boundByStmts	= Set.unions (map bindingVarsOfStmt ss)
	    in	freePerStmt \\ boundByStmts

	XIfThenElse _ x1 x2 x3	
 	 -> unions [freeVars x1, freeVars x2, freeVars x3]

	XLambdaTEC	_ v x t e c	-> freeVars x \\ singleton v

	XProjTagged 	_ vI vC x j		-> freeVars x
	XProjTaggedT	_ vI vC j		-> empty

	XVarInst a v			-> singleton v


-- Alt --------------------------------------------------------------------------------------------
instance FreeVars (Alt a) where
 freeVars aa 
  = case aa of
	AAlt n gs x	-> freeVarsOfAlt (freeVars x) (reverse gs)

freeVarsOfAlt :: Set Var -> [Guard a] -> Set Var
freeVarsOfAlt free []	= free

freeVarsOfAlt free (GCase _ w : gs)
	= freeVarsOfAlt (free \\ bindingVarsOfPat w) gs

freeVarsOfAlt free (GExp _ w x : gs)
	= freeVarsOfAlt 
		(unions [ free \\ bindingVarsOfPat w
			, freeVars x ])
		gs

-- Stmt -------------------------------------------------------------------------------------------
instance FreeVars (Stmt a) where
 freeVars ss
  = case ss of
	SBind _ Nothing x	-> freeVars x
	SBind _ (Just v) x	-> freeVars x \\ singleton v

	SBindMonadic _ w x	-> freeVars x \\ bindingVarsOfPat w
	SBindPat     _ w x	-> freeVars x \\ bindingVarsOfPat w
	SSig	     _ v t	-> freeVars t


