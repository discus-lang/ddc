
-- | Bits and pieces for working on Core.Exp.
--	These are the simplest utils at the bottom of the dependency tree.
--	They shouldn't depend on any other Core modules besides Exp.
--
module Core.Util.Bits

	-- predicates
	( isXApp
	, isXLambda
	, isXLAMBDA
	, isXTau
	, isCafP 

	-- projections	
	, takeVarOfStmt
	
	-- application
	, buildApp
	, flattenApps
	, flattenAppsE
	, unflattenAppsE
	, splitApps

	-- lambda		
	, chopLambdas
	, addLAMBDAs)
where

import Core.Exp

import Type.Util

import Shared.Error
import Shared.Var 		(NameSpace(..))
import Shared.Error		(panic)
import qualified Shared.Var as Var
import Shared.VarPrim

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Debug.Trace
import Util


-- Predicates --------------------------------------------------------------------------------------
isXApp x
	= or
	[ x =@= XAPP{}
	, x =@= XApp{} ]


isXLambda x	= (x =@= XLam{})
isXLAMBDA x	= (x =@= XLAM{})
isXTau x	= (x =@= XTau{})


-- | Check whether a top level thing is a CAF binding
isCafP :: Top -> Bool
isCafP	pp
 = case pp of
 	PBind v x	-> isCafX x
	_		-> False
	
isCafX xx
 = case xx of
	XAnnot n x	-> isCafX x

 	XLAM v k x	-> isCafX x
	XLam{}		-> False
	
	XTet vts x	-> isCafX x
	XTau t x	-> isCafX x
	
	XLocal v vs x	-> isCafX x
	
	_		-> True


-- Projections -------------------------------------------------------------------------------------
takeVarOfStmt :: Stmt -> Maybe Var
takeVarOfStmt ss
 = case ss of
 	SBind mv x	-> mv


-- Application -------------------------------------------------------------------------------------
-- | Flatten an expression application into a list
flattenApps :: Exp -> [Exp]
flattenApps xx
	| XAPP e1 e2	<- xx
	= flattenApps e1 ++ [XType e2]

	| otherwise
	= [xx]

-- | Create an application from a list of expressions
--	buildApp [x1, x2, x3] => (x1 x2) x3
buildApp :: [Exp] -> Maybe Exp
buildApp xx
	= buildApp'
	$ reverse xx

buildApp' xx
	| x : []		<- xx
	= Just x
	
	| XType t : xs		<- xx
	, Just leftX		<- buildApp' xs
	= Just $ XAPP leftX t
	
	| XVar v t : xs		<- xx
	, Var.nameSpace v == NameValue
	, Just leftX		<- buildApp' xs
	= Just $ XApp leftX (XVar v t) (TBot KEffect)

	| otherwise
	= Nothing

-- | Flatten type and value applications, recording which ones we have by XAppFP nodes.
flattenAppsE ::	Exp -> [Exp]
flattenAppsE x
	
	| XApp e1 e2 eff	<- x
	= flattenAppsE e1 ++ [XAppFP e2 (Just eff)]

	| XAPP  e1 e2		<- x
	= flattenAppsE e1 ++ [XAppFP (XType e2) Nothing]

	
	| otherwise
	= [XAppFP x Nothing]
	
-- | Build some type/value applications
unflattenAppsE :: [Exp]	-> Exp
unflattenAppsE	xx
	
	| x1:x2:xs			<- xx
	, XAppFP e1 Nothing		<- x1
	, XAppFP e2 (Just eff2)		<- x2
	
	= unflattenAppsE 
		$ [XAppFP (XApp e1 e2 eff2) Nothing] ++ xs
	
	| x1:x2:xs			<- xx
	, XAppFP e1           Nothing	<- x1
	, XAppFP (XType e2) Nothing	<- x2
	
	= unflattenAppsE 
		$ [XAppFP (XAPP e1 e2) Nothing] ++ xs
	
	| x1:[]				<- xx
	, XAppFP e1 Nothing		<- x1
	= e1


-- | Split out args and effects produced at each application
splitApps ::	Exp -> [(Exp, Effect)]
splitApps	x
 = case x of
 	XAPP e1 e2
	 -> splitApps e1 ++ [(XType e2, pure)]
	
	XApp e1 e2 eff
	 -> splitApps e1 ++ [(e2, eff)]
		
	_ -> [(x, pure)]
	
-- Lambda ------------------------------------------------------------------------------------------
-- | Chop the outer set of lambdas off a lambda expression and return the var-scheme pairs.
chopLambdas ::	Exp -> (Exp, [(Var, Type)])
chopLambdas	x

	| XLam v t e eff clo	<- x
	= let	(e', rest)	= chopLambdas e
	  in 	(e', (v, t) : rest)
	  
	| otherwise
	= (x, [])
	
-- | Add some type lambdas to an expression.	
addLAMBDAs ::	[(Bind, Kind)] -> Exp -> Exp
addLAMBDAs	vks x
 = case vks of
 	[]			-> x
	((v, k) : vks)		-> XLAM v k (addLAMBDAs vks x)

