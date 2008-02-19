
-- | Bits and pieces for working on Core.Exp.
--	These are the simplest utils at the bottom of the dependency tree.
--	They shouldn't depend on any other Core modules besides Exp.
--
module Core.Util.Bits
	( isXApp
	, isXLambda
	, isXLAMBDA
	, isXTau
	, isTForall
	
	-- projections
	, takeVarOfStmt
	
	, makeTSum,	flattenTSum
	, makeTMask,	applyTMask
	
	, makeTWhere
	, makeTFetters
	, makeTWitJoin
	, makeKWitJoin

	, kindOfSpace 
	, takeWitnessOfClass
	, pure
	, empty
	
	, buildApp
	, flattenApps,	flattenAppsE,	unflattenAppsE
	, flattenFun,	unflattenFun,	unflattenFunE
		
	, chopLambdas

	, isUnboxedT
	, crushClo
	, splitApps
	, isCafP 
	, typeToVar

	, addLAMBDAs
	
	, slurpVarsRD)


where

import Util
import Core.Exp
import Shared.Error
import Shared.Var 		(NameSpace(..))
import Shared.Error		(panic)
import qualified Shared.Var as Var
import Shared.VarPrim

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Debug.Trace
import Util

-----
pure	= TBot KEffect
empty	= TBot KClosure

stage	= "Core.Util.Bits"

isXApp x
	= or
	[ x =@= XAPP{}
	, x =@= XApp{} ]


isXLambda x	= (x =@= XLam{})
isXLAMBDA x	= (x =@= XLAM{})
isXTau x	= (x =@= XTau{})

isTForall x	= x =@= TForall{}

--------------------------------------------------------------------------------
-- Simple Projections
--
takeVarOfStmt :: Stmt -> Maybe Var
takeVarOfStmt ss
 = case ss of
 	SBind mv x	-> mv


--------------------------------------------------------------------------------
-- Make / Flatten / Apply functions
	
-- | make a sum from these things
--	return bottom if there aren't any
makeTSum :: Kind -> [Type] -> Type
makeTSum k ts
 = case nub $ catMap flattenTSum ts of
 	[]	-> TBot k
	[t]	-> t
	ts'	-> TSum k ts'


-- | flatten this sum into a list of things
flattenTSum :: Type -> [Type]
flattenTSum tt
 = case tt of
 	TBot k 		-> []
	TSum k ts	-> catMap flattenTSum ts
	_		-> [tt]

--
makeTMask :: Kind -> Type -> Type -> Type
makeTMask k t1 t2
 = case t2 of
 	TBot KClosure	-> t1
	_		-> applyTMask $ TMask k t1 t2


-- | Crush a TMask by discarding TFree and TEffects 
--	in the first term which are present in the second.
applyTMask :: Type -> Type
applyTMask tt@(TMask k t1 t2)
 = let	vsKill	= map (\t -> case t of
 				TFree v t	-> v
				TTag  v		-> v
				_		
				 -> panic stage 
				 	$ "applyTMask: no match for " % show t % "\n"
				  	% "  tt = " % show tt % "\n")
		$ flattenTSum t2

	tsMasked
		= map (\t -> case t of
				TFree v tr	
				 | v `elem` vsKill	-> TBot k
				 | otherwise		-> TFree v tr

				_			-> TMask k t t2)
		$ flattenTSum t1

   in	makeTSum k tsMasked
 

-- 
makeTWitJoin :: [Type] -> Type
makeTWitJoin ts
 = case ts of
 	[t]	-> t
	ts	-> TWitJoin ts

-- 
makeKWitJoin :: [Kind] -> Kind
makeKWitJoin ts
 = case ts of
 	[t]	-> t
	ts	-> KWitJoin ts

		
makeTWhere ::	Type	-> [(Var, Type)] -> Type
makeTWhere	t []	= t
makeTWhere	t vts	= TFetters t $ map (uncurry FWhere) vts


makeTFetters :: Type -> [Fetter] -> Type
makeTFetters t []	= t
makeTFetters t fs	= TFetters t fs
		
		
		
-- | Get the kind associated with a namespace.
kindOfSpace :: NameSpace -> Maybe Kind
kindOfSpace space
 = case space of
 	NameType	-> Just KData
	NameRegion	-> Just KRegion
	NameEffect	-> Just KEffect
	NameClosure	-> Just KClosure
	_		-> Nothing

--	_		-> panic stage
--			$  "kindOfSpace: no match for " % show space


-- | Build the witness needed to satify this constraint.
takeWitnessOfClass :: Kind -> Maybe Type
takeWitnessOfClass kk
 = case kk of
 	KClass v ts	-> Just (TClass v ts)
	_		-> Nothing
	


flattenApps :: Exp -> [Exp]
flattenApps xx
	
	| XAPP e1 e2	<- xx
	= flattenApps e1 ++ [XType e2]

	| otherwise
	= [xx]


-- | Create an application from a list of expressions
--	buildApp [x1, x2, x3] => (x1 x2) x3
--
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
	
	
-----
flattenFun ::	Type -> [Type]
flattenFun	xx
 = case xx of
	TFunEC e1 e2 _ _	-> e1 : flattenFun e2
	TFun   e1 e2		-> e1 : flattenFun e2
	_			-> [xx]

unflattenFun ::	[Type] 	-> Type
unflattenFun	xx
 = case xx of
 	x:[]		-> x
	x:xs		-> TFun x (unflattenFun xs)


-----
unflattenFunE :: [Type] -> Type
unflattenFunE xx
 = case xx of
 	x : []		-> x
	x : xs		-> TFunEC x (unflattenFunE xs) pure empty


-----
flattenAppsE ::	Exp	-> [Exp]
flattenAppsE	x
	
	| XApp e1 e2 eff	<- x
	= flattenAppsE e1 ++ [XAppFP e2 (Just eff)]

	| XAPP  e1 e2		<- x
	= flattenAppsE e1 ++ [XAppFP (XType e2) Nothing]

	
	| otherwise
	= [XAppFP x Nothing]
	


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
	

-----------------------
-- chopLambdas
--	Chop the outer set of lambdas off a lambda expression and
--	return the var-scheme pairs.
--	
chopLambdas ::	Exp -> (Exp, [(Var, Type)])
chopLambdas	x

	| XLam v t e eff clo	<- x
	= let	(e', rest)	= chopLambdas e
	  in 	(e', (v, t) : rest)
	  
	| otherwise
	= (x, [])


isUnboxedT :: Type -> Bool
isUnboxedT t
 = case t of
 	TData v _
	 | last (Var.name v) == '#'	-> True	 
	_				-> False


crushClo :: Closure -> [Closure]
crushClo cc
 = case cc of
 	TBot KClosure		-> []
	TSum KClosure cs	-> catMap crushClo cs
	_			-> [cc]




-----
splitApps ::	Exp -> [(Exp, Effect)]

splitApps	x
 = case x of
 	XAPP e1 e2
	 -> splitApps e1 ++ [(XType e2, pure)]
	
	XApp e1 e2 eff
	 -> splitApps e1 ++ [(e2, eff)]
		
	_ -> [(x, pure)]


	
	
-----
isCafP :: Top -> Bool
isCafP	pp
 = case pp of
 	PBind v x	-> isCafX x
--	PSuper v x	-> isCafX x
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
	

typeToVar :: Type -> Maybe Var
typeToVar tt
 = case tt of
	TVar k v		-> Just v
	_			-> Nothing
	
	
addLAMBDAs ::	[(Bind, Kind)] -> Exp -> Exp
addLAMBDAs	vks x
 = case vks of
 	[]			-> x
	((v, k) : vks)		-> XLAM v k (addLAMBDAs vks x)


-- | Slurp out the region and data vars present in this type
--	Used for crushing ReadT, ConstT and friends
slurpVarsRD
	:: Type 
	-> ( [Region]	-- region vars and cids
	   , [Data])	-- data vars and cids

slurpVarsRD tt
 = 	slurpVarsRD_split [] [] $ slurpVarsRD' tt

slurpVarsRD_split rs ds []	= (rs, ds)
slurpVarsRD_split rs ds (t:ts)
 = case t of
 	TVar   KRegion _	-> slurpVarsRD_split (t : rs) ds ts
 	TVar   KData _		-> slurpVarsRD_split rs (t : ds) ts
	
	_			-> slurpVarsRD_split rs ds ts
	
slurpVarsRD' tt
 = case tt of
	TFun{}			-> []
 	TData v ts		-> catMap slurpVarsRD' ts

	TVar KRegion _		-> [tt]
	TVar KData   _		-> [tt]
	TVar _  _		-> []

	_ 	-> panic stage
		$  "slurpVarsRD: no match for " % show tt % "\n"


