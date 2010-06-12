
-- | Bits and pieces for working on Core.Exp.
--	These are the simplest utils at the bottom of the dependency tree.
--	They shouldn't depend on any other Core modules besides Exp.
--
module Core.Util.Bits
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
	, splitAppsUsingPrimType

	-- lambda		
	, chopLambdas
	, addLAMBDAs)
where
import DDC.Core.Exp
import DDC.Type
import DDC.Var
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
 	XLAM v k x	-> isCafX x
	XLam{}		-> False
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
flattenApps :: Exp -> [Either Exp Type]
flattenApps xx
	| XAPP x t	<- xx
	= flattenApps x ++ [Right t]

	| otherwise
	= [Left xx]

-- | Create an application from a list of expressions
--	buildApp [x1, x2, x3] => (x1 x2) x3
buildApp :: [Either Exp Type] -> Maybe Exp
buildApp xx
	= buildApp'
	$ reverse xx

buildApp' xx
	| Left x : []		<- xx
	= Just x
	
	| Right t : xs		<- xx
	, Just leftX		<- buildApp' xs
	= Just $ XAPP leftX t
	
	| Left (XVar v t) : xs	<- xx
	, varNameSpace v == NameValue
	, Just leftX		<- buildApp' xs
	= Just $ XApp leftX (XVar v t)

	| otherwise
	= Nothing

-- | Flatten type and value applications.
--   For value applications we get the expression and effect cause by that application.
--   For type  applications we just get the type.
flattenAppsE ::	Exp -> [Either Exp Type]
flattenAppsE x
	| XApp e1 e2	<- x
	= flattenAppsE e1 ++ [Left e2]

	| XAPP  x t		<- x
	= flattenAppsE x ++ [Right t]

	| otherwise
	= [Left x]
	
	
-- | Build some type/value applications
unflattenAppsE :: [Either Exp Type] -> Exp
unflattenAppsE	xx
	
	| x1:x2:xs	<- xx
	, Left e1	<- x1
	, Left e2	<- x2
	= unflattenAppsE 
	$ [Left $ XApp e1 e2] ++ xs
	
	| x1:x2:xs	<- xx
	, Left  e1	<- x1
	, Right e2	<- x2
	= unflattenAppsE 
	$ [Left $ XAPP e1 e2] ++ xs
	
	| x1:[]		<- xx
	, Left e1	<- x1
	= e1


-- | Split out args and effects produced at each application
splitApps :: Exp -> [Either Exp Type]
splitApps xx
 = case xx of
 	XAPP e1 e2
	 -> splitApps e1 ++ [Right e2]
	
	XApp e1 e2 
	 -> splitApps e1 ++ [Left e2]
		
	_ -> [Left xx]
	
-- hacks
splitAppsUsingPrimType :: Exp -> [Exp]
splitAppsUsingPrimType xx
 = case xx of
 	XAPP e1 e2
	 -> splitAppsUsingPrimType e1 ++ [XPrimType e2]
	
	XApp e1 e2
	 -> splitAppsUsingPrimType e1 ++ [e2]
		
	_ -> [xx]
	
	
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

