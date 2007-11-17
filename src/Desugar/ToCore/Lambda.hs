
module Desugar.ToCore.Lambda
	( fillLambdas
	, addTau )
where

import Util
import qualified Debug.Trace	as Debug

import qualified Data.Set 	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var 	as Var
import Shared.Var		(VarBind, NameSpace(..))
import Shared.Error

import Core.Exp
import Core.Util
import Core.Pack
import Core.Util.Substitute
import Core.Util.Strip

import Desugar.ToCore.Base

import qualified Type.ToCore	as T
import qualified Type.Exp	as T

-----
stage 		= "Desugar.ToCore.Lambda"
debug		= True
trace ss x	= if debug 
			then Debug.trace (pretty ss) x
			else x

-----------------------
-- fillLambdas
--	Track which where's have been added to the type, and substitute 
--	the rest at the end to ensure that all information is in.
--	

fillLambdas
	:: Var
	-> Type
	-> Exp
	-> CoreM Exp
	
fillLambdas v tScheme x
 =  {- trace 
 	("* Desugar.ToCore.fillLambdas\n"
 	% "    v       = " % v % 	"\n"
	% "    tScheme =\n" %> tScheme %	"\n")  -}
	(fillLambdas' v Map.empty tScheme x)	
	
	
fillLambdas' v tsWhere tScheme x
 	| TForall v k tRest		<- tScheme
	= do	x'	<- fillLambdas' v tsWhere tRest x
		return	$ XLAM v k x'
	
	-- Give this witness a new name so we can refer to it later on.
	| TContext c tRest		<- tScheme
	= do	x'	<- fillLambdas' v tsWhere tRest x
		v'	<- newVarN NameClass
		return	$ XLAM v' c x'

	| TWhere tRest vts		<- tScheme
	= do	let tsWhere'	= Map.union tsWhere (Map.fromList vts)
		x'	<- fillLambdas' v tsWhere' tRest x
		return	$ XTet vts x'

	| TFunEC t1 t2 eff clo		<- tScheme
	, XLam v _ x' _ _		<- x
 	= do	x2		<- fillLambdas' v tsWhere t2 x'
		return	$ XLam v t1 x2 eff clo

	| otherwise
	= return	$ addTau tScheme x



addTau xT x
	| gotAnnot x	= x
	| otherwise	= XTau xT x
	
gotAnnot x
 = case x of
 	XLAM{}		-> True
	XLam{}		-> True
	XTau{}		-> True
	XTet vts x	-> gotAnnot x
	_		-> False




-----------------------
-- addTypeApps
--	| See how this let-bound var was instantiated and add
--	  type/region/effect applications.
--
{-
addTypeApps 
	:: Var 		-- ^ The value var.
	-> Var 		-- ^ The type var for this value var.
	-> Exp 		-- ^ The Exp holding the value var, eg (XVar v).
			--	we want to apply the type applications to this expression.
	-> CoreM Exp

addTypeApps v vT exp
 = do
	-- Grab the type scheme for this var.
 	tScheme		<- getType v

	-- See how the scheme was instantiated.
	Just instInfo	<- lookupInst vT

	trace
	 (pretty	$ "--- addTypeApps\n"
	 		% " v                = "	 % v		% "\n"
			% " vT               = "	 % vT		% "\n"
			% " exp              = "	 % exp		% "\n\n"
			% " tScheme          = \n"	 %> tScheme	% "\n\n"
			% " mInstVs          = "	 % mInstVs	% "\n")
	 $ 
	addTypeApps2 v vT exp tScheme instInfo
	 

-- No scheme was instantiated when this var was used
--	This will happen at the point when a recursive function calls itself.
--	Pass the quantified variables back into ourselves.

addTypeApps2 v vT exp tScheme (T.InstanceLetRec v1 v2 _)
 = do 	let (forallVTs, _, classes, tRest)	
 				= stripSchemeT tScheme
	let forallVs		= map fst forallVTs

	let tsInst		= map 	(\v -> 	let Just t	= varToType v
						in  t) 
					forallVs

	let expFinal		= unflattenApps (exp 
					:  map (\t -> XType $ t) tsInst
					++ map (\c -> XType $ c) classes)

	let subL		= zip forallVs tsInst
	let sub			= Map.fromList subL
	let tRest'		= substituteT sub tRest
	let classes'		= map (substituteT sub) classes
	
	addTypeApps3 expFinal tRest'

-- The scheme was instantiated at this point
-- 	First, puncture the inst to throw out application of local 
--	effect and closure information. We'll pass this information via a TLet instead.
--	
addTypeApps2 v vT exp tScheme (T.InstanceLet vUse vBind tsInst tScheme_)
 = do
	-- get the original scheme in Type representation
	sigmaTable		<- gets coreSigmaTable
	schemeMapT		<- gets coreMapTypes
	let (Just funVarT)	= Map.lookup v sigmaTable
	let (Just tSchemeT)	= Map.lookup funVarT schemeMapT

	-- work out how to puncture the inst vars
	let pMap		= T.punctureInstMapT tSchemeT
	let tsInst_punctured	= T.punctureInst pMap tsInst

	-- We can safely erase the contexts of arguments in type applications.
	--	If the function being called needs a certain witness then they will
	--	be passed in separately. The fact that a given object may have more
	--	context doesn't matter to the called function.
	--
	let tsInst_noContext	= map eraseContextsT tsInst_punctured

	-- strip the foralls off the scheme
	let (forallVTs, _, classes, tRest)
				= stripSchemeT tScheme

	-- substitute into type
	let forallVs		= map fst forallVTs
	let sub			= Map.fromList $ zip forallVs tsInst_noContext
	let tRest'		= substituteT sub tRest
	let classes'		= map (substituteT sub) classes

	let expFinal		= unflattenApps (exp 
					:  map (\t -> XType t) tsInst_noContext
					++ map (\c -> XType c) classes')
	
	trace
	 (pretty	$ " tsInst_punctured = "	% tsInst_punctured	% "\n"
			% " tsInst_noContext = "	% tsInst_noContext	% "\n"
			% " forallVTs        = " 	% forallVTs		% "\n"
			% " sub              = "	% show sub		% "\n\n")
	 $ 
	addTypeApps3 expFinal tRest'


addTypeApps3 :: Exp -> Type  -> CoreM Exp
addTypeApps3 expFinal tRest
 = do
	trace
	 (pretty	$ " expFinal         = \n"	%> expFinal		% "\n\n"
	 		% " tRest            = \n"	%> show tRest		% "\n\n")
	 $ 
	return expFinal

-}
