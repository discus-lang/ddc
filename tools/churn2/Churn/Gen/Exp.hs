
module Churn.Gen.Exp
where

import Churn.Gen.Base
import Churn.Bits
import Churn.Type
import Churn.Env

import Shared.Base
import Shared.VarPrim
import Source.Exp
import Type.Exp
import Util.List

import Control.Monad

-- | Generate an expression
genExp :: Env -> Fuel -> Type -> GenM (Exp Type)
genExp env fuel tt
	| fuel >= 4
	= do	gen	<- genChoose 
				[ genExp_if env fuel tt
				, genExp_do env fuel tt ]
		gen
		
	| tt	== tInt 
	= genExp_LitInt	
	
	| tt	== tBool
	= genExp_LitBool


-- | Generate a let expression
genExp_do :: Env -> Fuel -> Type -> GenM (Exp Type)
genExp_do env fuel tt
 = do	bindSmallness	<- genRandomR (1, 5)
	bindCount	<- genRandomR (1, fuel `div` bindSmallness)
	(fexp:fbinds)	<- genSplitFuel fuel (bindCount + 1)
	
	-- bind 80 percent of vars
	let [nVars, nNoVars] 
			= drain bindCount [5, 5]
	
	vars		<- replicateM nVars freshVar	
	let mVars	= replicate nNoVars Nothing
			++ [Just v | v <- vars]

	let mVarFuels	= zip mVars fbinds
	
	(env', stmts)	<- mapAccumLM genStmt env mVarFuels
		
	-- the body expression.
	exp		<- genExp env' fexp tt

	return	$ XDo tt 
			(  stmts
			++ [SStmt tt exp])
			
-- | Generate an if-then-else expression
genExp_if :: Env -> Fuel -> Type -> GenM (Exp Type)
genExp_if env fuel tt
 = do	[fb, fx1, fx2]	
		<- genSplitFuel fuel 3

	b	<- genExp env fb  tBool
	x1	<- genExp env fx1 tt
	x2	<- genExp env fx2 tt
	return	$ XIfThenElse tt b x1 x2
	

-- | Generate a literal integer.
genExp_LitInt :: GenM (Exp Type)
genExp_LitInt
 = do	n	<- genRandomR (0, 100)
	return	$ XLit tInt (LiteralFmt (LInt n) Boxed)


-- | Generate a literal boolean
genExp_LitBool :: GenM (Exp Type)
genExp_LitBool
 = do	n	<- genRandomR (0, 1)
	if n == (0 :: Int)
	 then	return	$ XVar tBool primTrue
	 else	return	$ XVar tBool primFalse
	
-- | Generate a statement
--	If Just var, then bind the var, otherwise not.
genStmt :: Env -> (Maybe Var, Fuel) -> GenM (Env, Stmt Type)
genStmt env (mVar, fuel) 
 = do	let tt	= tInt

	exp	<- genExp env fuel tt

	case mVar of
	 Just var 
	   -> return	$ (env, SBindFun tt var [] exp)

	 Nothing
	   -> return	$ (env, SStmt tt exp)
	

-- | If this statement binds a variable then take it
--	and its type.
takeStmtVarType :: Stmt Type -> Maybe (Var, Type)
takeStmtVarType ss
 = case ss of
	SStmt{}			-> Nothing
	SBindFun tt v ps x	-> Just (v, tt)
	-- TODO: rest of binding forms.
	


