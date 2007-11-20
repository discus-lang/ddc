
module Core.Lift.LambdaFree
 	( lambdaFreeVarsP )
where

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

-----
import Util

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))
import qualified Shared.VarUtil	as Var

import Core.Exp
import Core.Util
import Core.Pretty

import Core.Plate.FreeVars
import Core.Lift.Base
import qualified Debug.Trace	as Debug

debug		= False

trace ss x	
 = if debug
 	then	Debug.trace ss x
	else	x

-----
lambdaFreeVarsP 
	:: Top
	-> LiftM 
		( Top
		, [(Var, Type)]		-- type/region/effect/class vars bound
		, [(Var, Type)])	-- value vars bound
		

lambdaFreeVarsP
	(PBind v x)
 = do
	-- Work out the free variables in this binding
	vsBoundTop	<- gets stateTopVars

 	let vsFree	= Set.difference (freeVarsX x) vsBoundTop
{-	let vsFreeV	= filter (\v -> Var.nameSpace v == NameValue) 
			$ Set.toList vsFree

	-- We'll be binding the types of free vars, which might contain TREC vars.
	-- These need to be passed to, so check the types for free vars also.
	tsFreeV		<- mapM getType vsFreeV

	let vsFreeV_types 	
			= Set.difference (Set.unions $ map freeVarsT tsFreeV)
			vsBoundTop
	
	let freeX	= Set.toList $ Set.union vsFree vsFreeV_types
-}
--	let freeX	= vsFree	

	-- The vars defined at top-level are always in scope.
	let freeVars	= Set.toList vsFree

	-- Partition free vars in to Value and Type/Region/Effect vars
	let (freeVs, freeTREs)
			= partition (\v -> Var.nameSpace v == NameValue)
			$ freeVars

	freeKs		<- mapM getKind freeTREs
	let freeVKs	= zip freeTREs $ map TKind freeKs

	freeTs		<- mapM getType freeVs

	-- strip any context and tets from the arg types, 
	--	we'll add these on the front of the actual super
--	let (tetcs, freeTs2)	= unzip $ map stripArgType freeTs
--	let (tets, classess)	= unzip tetcs
--	let tsClasses		= concat classess

	let freeVTs	= zip freeVs freeTs

--	classVs 		<- replicateM (length tsClasses) (newVar NameClass) 
--	let classVTs	= zip classVs tsClasses
	
	-- Add new lambdas to bind all the free vars
	let xLambdas	= addLAMBDAs freeVKs
--			$ addLAMBDAs classVTs
			$ addLambdas freeVTs x

	-- all done
	let pBound	= PBind v xLambdas
	let Just tBound	= maybeSlurpTypeX xLambdas

	-- add the new type to the liftM state straight away, so if we had nested lambda bindings,
	--	the next time we add lambdas we'll know what the type of this one was.
	bindType v tBound
	

	trace
	 (pretty	$ "* bindFreeVarsP\n"
			% "    v             (var of binding being lifted) = " % v % "\n"
			% "    vsFree        (vars free in binding)        = " % Set.toList vsFree	% "\n"
--			% "    vsFreeV       (value vars free in binding)  = " % vsFreeV	% "\n"
--                        % "    tsFreeV       (types of vsFreeV)            = " % tsFreeV	% "\n"
--			% "    vsFreeV_types (free vars of type of vsFree) = " % Set.toList vsFreeV_types	% "\n"
--	 		% "    freeX         (vars free in binding)        = " % (map Var.bind $ freeX) % "\n\n"
			% "    freeVKs       (type vars free)              = " % freeVKs  	% "\n"
--			% "    classVTs      (class vars free)             = " % classVTs	% "\n"
			% "    freeVTs       (value vars free)             = " % freeVTs	% "\n\n"
			% "    tBound        (new type for binding)         =\n"
			%> tBound % "\n")

	 $ return 
	 	( pBound
	 	, freeVKs -- ++ classVTs
		, freeVTs)

stripArgType	:: Type -> ( ([(Var, Type)], [Class]), Type)
stripArgType t
 = let	(forallVTs, _, classes, tt)
 		= stripSchemeT t
		
   in	(([], classes), buildScheme forallVTs [] [] tt)


-----------------------
-- stripLambdas
-- |	strip the lambdas off an expression
--
stripLambdas ::	Exp -> ([(Var, Type)], [(Var, Type)], Exp)
stripLambdas	x
 = case x of
 	XLam v t x eff clo	
	 -> let	(vks, vts, x')		= stripLambdas x
	    in	(vks, (v, t) : vts, x')
	    
	XLAM v k x
	 -> let (vks, vts, x')		= stripLambdas x
	    in	((v, k) : vks, vts, x')

	_ -> ([], [], x)




