-- Core.Reconstruct
--	Reconstruct witness passing
--	and effect annotations on apps, match and do's
--
--	Change this to type checking of core.
--	Work out type and effect for any core term.

module Core.Reconstruct
	( reconstructTree )

where

import Core.Exp
import Core.Util
import Core.Util.Substitute
import Core.Util.Slurp
import Core.Pack
import Core.Plate.FreeVars
import Core.Util.Strip
import Util.Graph.Deps
import Core.Util.Effect		(crushEffsT)

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Shared.Error

import qualified Debug.Trace	as Debug


-----
stage	= "Core.Reconstruct"

debug		= True
trace ss x	= if debug then Debug.trace (pretty ss) x else x

reconstructTree
	:: Tree		-- header tree 
	-> Tree 	-- core tree
	-> Tree		-- core tree with reconstructed type information
	
reconstructTree tHeader tCore
 = let	
 	-- slurp out all the stuff defined at top level
	topTypes	= catMap slurpTypesP (tHeader ++ tCore)
 	tt		= foldl addVT Map.empty topTypes
	
	-- reconstruct type info on each top level thing
	tCore'		= map (reconP tt) tCore
	
   in	tCore'
	

addVT :: Map Var Type -> (Var, Type) -> Map Var Type
addVT tt (v, t)	 
 = {- trace 
 	( "addVT: " % v 	% "\n"
 	% "   t = " %> t 	% "\n") -}
	 case Map.lookup v tt of
	 	Nothing	-> Map.insert v t tt
		Just _	-> Map.insert v t (Map.delete v tt)
	

-----
reconP :: Map Var Type -> Top -> Top

reconP tt (PBind v x)
 = let	(x', xt)	= reconX tt x
   in	PBind v x'

reconP tt p		= p


-- | Reconstruct effect and witness info on this expression
reconX 	:: Map Var Type 	-- ^ var -> type 
	-> Exp 			-- ^ expression to reconstruct info on
	-> ( Exp		-- expression with reconstructed info
	   , Type)		-- type of expression

reconX tt (XLAM v t x)
 = let	tt'		= addVT tt (v, t)
 	(x', tx)	= reconX tt' x
   in	( XLAM 	  v t x'
    	, TForall v t tx )
   
reconX tt exp@(XAPP x t)
 = let	(x', tx)	= reconX tt x
   in	case applyTypeT tx t of
   	 Just t'	-> (XAPP x' t, t')
	  
	 _ -> panic stage
	 	$ "reconX: Kind error in type application (x t).\n"
		% "     x     =\n" %> x		% "\n\n"
		% "     t     =\n" %> t		% "\n\n"
		% "   T[x]    =\n" %> tx	% "\n\n"

reconX tt (XTet vts x)
 = let	tt'		= foldl' addVT tt vts
 	(x', tx)	= reconX tt' x
   in	( XTet   vts x'
   	, TWhere tx vts)
   
-- BUGS: we should check the XTau type here   
reconX tt (XTau t x)
 = let	(x', tx)	= reconX tt x
   in	( XTau t x'
   	, t)

reconX tt (XLam v t x eff clo)
 = let	tt'		= addVT tt (v, t)
 	(x', tx)	= reconX tt' x
   in	( XLam v t x' eff clo
   	, TFunEC t tx eff clo)

reconX tt (XLocal v vs x)
 = let	tt'		= addVT tt (v, TKind KRegion)
 	(x', tx)	= reconX tt x
   in	( XLocal v vs x'
   	, tx)

reconX tt exp@(XApp x1 x2 eff)
 = let	(x1', x1t)	= reconX tt x1
	(x2', x2t)	= reconX tt x2

   in	case applyValueT x1t x2t of
 	 Just (t, eff')
	  -> let x'		= XApp x1' x2' (narrowT eff')
     	     in	 (x', t)

	 _ -> panic stage	
	 	$ "reconX: Type error in value application (x1 x2).\n"
		% " in expression:\n"
		% "     (" % x1 % ") " % x2	% "\n\n"

		% "   T[x1]   = " % x1t		% "\n\n"
		% "   T[x2]   = " % x2t		% "\n\n"
   
reconX tt (XDo ss)
 = let	(tt', sts)	= mapAccumL reconS tt ss
	(ss', ts)	= unzip sts
	Just t		= takeLast ts
	
   in	(XDo ss', t)
   
   
-- BUGS: also fill in effect information here
reconX tt (XMatch aa eff)
 = let	(aa', ats)	= unzip $ map (reconA tt) aa
 	Just atLast	= takeLast ats
   in	( XMatch aa' eff
   	, atLast)
	
reconX tt (XConst c t)
 =	( XConst c t
 	, t)
	

-- BUGS: substitute vars into type
reconX tt (XVar v)
 = case Map.lookup v tt of
 	Just t		
	 -> {- trace 
	 	( "reconX: (XVar " % v % ")\n"
	 	% "    t =\n" %> t	% "\n") -}
		(XVar v, t)
	
	Nothing		-> panic stage $ "reconX: Variable " % v % " is not bound"

reconX tt xx@(XPrim prim xs eff)
 = let	Just t	= maybeSlurpTypeX xx
   in	( xx, t)
	
reconX tt xx
 	= panic stage 
 	$ "reconX: no match for " ++ show xx


-----------------------
--
reconS :: Map Var Type -> Stmt -> (Map Var Type, (Stmt, Type))

reconS tt (SBind Nothing x)	
 = let	(x', tx)	= reconX tt x
   in	( tt
   	, ( SBind Nothing (addTauX (narrowT tx) x')
	  , tx))

reconS tt (SBind (Just v) x)
 = let	(x', tx)	= reconX tt x
	tt'		= addVT tt (v, tx)
   in	( tt'
   	, (SBind (Just v) (addTauX (narrowT tx) x')
	  , tx))
	  

-----------------------
--
reconA :: Map Var Type -> Alt -> (Alt, Type)

reconA tt (AAlt gs x)
 = let	(tt', gs')	= mapAccumL reconG tt gs
	(x', tx)	= reconX tt' x
   in	( AAlt gs' x'
   	, tx)
  
 
-----------------------
--
reconG :: Map Var Type -> Guard -> (Map Var Type, Guard)

reconG tt (GExp p x)
 = let	binds		= slurpVarTypesW p
 	tt'		= foldl addVT tt binds
	(x', xt)	= reconX tt' x
   in	(tt', GExp p x')
 

slurpVarTypesW (WConst{})	= []
slurpVarTypesW (WCon v lvt)	= map (\(l, v, t)	-> (v, t)) lvt


-- | Work out the result type and latent effect that will result when 
--	an arg is applied to a function with this type.
--
--	BUGS: check that the types match out as we apply
--
applyValueT 
	:: Type 		-- ^ type of function
	-> Type 		-- ^ type of arg
	-> Maybe 		
		( Type		-- result type
		, Effect)	-- effect caused

applyValueT (TContext t1 t2) t3

	| Just (t', eff)	<- applyValueT t2 t3
	= Just  ( TContext t1 t'
		, eff)		-- don't create contexts for effects.

applyValueT (TWhere t2 vts) t		
	| Just (t2', eff)	<- applyValueT t2 t
	= Just  ( TWhere t2' vts
		, TWhere eff vts)

applyValueT t0@(TFunEC t1 t2 eff clo) t3	
	| t1_flat	<- crushEffsT $ inlineTWheresT Map.empty t1
	, t3_flat	<- crushEffsT $ inlineTWheresT Map.empty t3
	= if t1_flat == t3_flat
		then Just (t2, eff)
		else freakout stage
			( "applyType: Type error in value application.\n"
			% "    can't apply (" % t3 % ")\n"
			% "             to (" % t0 % ")\n"
			% "\n"
			% "    t1_flat = " % t1_flat % "\n"
			% "\n"
			% "  does not match\n"
			% "    t3_flat = " % t3_flat % "\n")
			$ Nothing
	
applyValueT _ t
	= Nothing
	
	
-----
-- applyTypeT
--	Apply a value argument to a forall/context type, yielding the result type.
--	BUGS: check that the types/contexts match as we apply.
--
applyTypeT :: Type -> Type -> Maybe Type

applyTypeT (TForall v k t1) t
	= Just (substituteT (Map.insert v t Map.empty) t1)
	
applyTypeT (TContext t1 t2) t
	= Just t2
	
applyTypeT (TWhere t1 vts) t
	| Just t1'	<- applyTypeT t1 t
	= Just $ TWhere t1' vts
	
applyTypeT t1 t2
	= panic stage $ "applyType: can't apply (" % t2 % ") to (" % t1 % ")"

-- | Throw out context and type bindings that aren't visible from the shape
--	of this type. All context and bindings need to be at top level.
--
narrowT :: Type -> Type
narrowT t
 = let	-- strip the scheme into its parts
 	(forallVTs, bindVTs, contexts, tShape)
 			= stripSchemeT t

	-- the type vars free in the shape
	shapeVs		= freeVarsT tShape
		
	-- construct a map of what vars are free in each of the type bindings.
	depMap		= Map.fromList 
			$ map (\(v, t) -> (v, freeVarsT t))
			$ bindVTs
	
	-- work out which of the bindings can be reached from the shape
	reachVs		= graphReachableS depMap
			$ shapeVs

	-- throw out unreachable bindings
	bindVTs'	= [ (v, t)	| (v, t)	<- bindVTs
	 				, Set.member v reachVs]
	
	-- rebuild the scheme 
	t'		= subTLet1
			$ buildScheme forallVTs bindVTs' contexts tShape
	
  in	{- trace 	( "narrowT\n"
		% "    t       = \n"	%> t			% "\n"
  		% "    shape   = "	% shape			% "\n"
		% "    shapeVs = "	% Set.toList shapeVs	% "\n"
		% "    reachVs = "	% Set.toList reachVs	% "\n\n"
		% "    t'      = \n"	%> t'			% "\n")  -}
		
		t'
		

-----
-- subTLet1
--	If this type is just a (let v = t in v)  then convert it to t.

subTLet1 ::	Type -> Type

subTLet1 (TForall v t x)
	= TForall v t (subTLet1 x)

subTLet1 (TContext c t)
	= TContext c (subTLet1 t)

subTLet1 (TWhere (TVar k v1) [(v2, t)])
	| v1 == v2	= t

subTLet1	t	= t


-----
-- addTauX
--	Have a look at this expression and see if we're going to be able to
--	slurp out a type for it with maybeSlurpTypeX. If so, leave it as it is, if not
--	attach an XTau to the front so that maybeSlurpTypeX will work next time around.

addTauX :: Type -> Exp -> Exp

addTauX t x
 = case maybeSlurpTypeX x of
 	Nothing	-> XTau t x
	Just _	-> x
