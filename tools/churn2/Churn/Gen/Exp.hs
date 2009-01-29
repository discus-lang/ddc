
module Churn.Gen.Exp
where

import Churn.Gen.Base
import Churn.Bits
import Churn.Type

import Shared.Base
import Shared.VarPrim
import Source.Exp
import Type.Exp

import qualified Data.Map	as Map
import Data.Map			(Map)

-- The type environment
data Env
	= Env {	envType :: Map Var Type }

initEnv
	= Env { envType	= Map.empty }

-- | Generate an expression
genExp :: Env -> Fuel -> Type -> GenM (Exp Type)
genExp env fuel tt
	| fuel >= 4
	= genExp_If env fuel tt
		
	| tt	== tInt 
	= genExp_LitInt	
	
	| tt	== tBool
	= genExp_LitBool


-- | Generate an if-then-else expression
--	Make the 
genExp_If :: Env -> Fuel -> Type -> GenM (Exp Type)
genExp_If env fuel tt
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
	
