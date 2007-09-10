-- Core.Reconstruct
--	Reconstruct type annotations on intermediate bindings.
--
module Core.Reconstruct
(
	reconstructTree
)

where

import Util
import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Shared.Error

import Core.Exp
import Core.Pretty
import Core.Util
import Core.Pack
import Core.Plate.Walk
import Core.Util.Unify	(matchC2)
import Core.Util.Substitute
import Core.Util.Slurp
import Core.Plate.FreeVars
import Core.Util.Strip
import Util.Graph.Deps
import qualified  Core.Plate.Trans	as Trans
import qualified Debug.Trace		as Debug


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


-----------------------
-- reconX
--
reconX :: Map Var Type -> Exp -> (Exp, Type)

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
	 	$ "reconX: Type error in type application (x t).\n"
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
		% "   exp     = " % exp 	% "\n\n"
		% "     x1    = " % x1		% "\n\n"
		% "   T[x1]   = " % x1t		% "\n\n"
		% "     x2    = " % x2		% "\n\n"
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
   	, ( SBind Nothing $ addTauX (narrowT tx) x'
	  , tx))

reconS tt (SBind (Just v) x)
 = let	(x', tx)	= reconX tt x
	tt'		= addVT tt (v, tx)
   in	( tt'
   	, (SBind (Just v) $ addTauX (narrowT tx) x'
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
	

	
-----
-- applyValueT
--	Apply a value argument to a function type, yielding the result type.
--	BUGS: check that the types match out as we apply
--
applyValueT :: Type -> Type -> Maybe (Type, Effect)

applyValueT (TContext t1 t2) t3
	| Just (t', eff)	<- applyValueT t2 t3
	= Just (TContext t1 t', TContext t1 eff)

applyValueT (TWhere t2 vts) t		
	| Just (t2', eff)	<- applyValueT t2 t
	= Just (TWhere t2' vts, TWhere eff vts)

applyValueT (TFunEC t1 t2 eff clo) t	
	= Just (t2, eff)
	
applyValueT _ t
	= Nothing


-----
-- applyTypeT
--	Apply a value argument to a forall/context type, yielding the result type.
--	BUGS: check that the types/contexts match as we apply.
--
applyTypeT :: Type -> Type -> Maybe Type
{-
applyTypeT (TLet v t1 t2) t
	| Just t2'		<- applyTypeT t2 t
	= Just (TLet v t1 t2')
-}
applyTypeT (TForall v k t1) t
	= Just (substituteT (Map.insert v t Map.empty) t1)
	
applyTypeT (TContext t1 t2) t
	= Just t2
	
applyTypeT t1 t2
	= panic stage $ "applyType: can't apply (" % t2 % ") to (" % t1 % ")"

-----
-- narrowT
--	Throw out any TLets from this type which aren't actually reachable.
--
narrowT :: Type -> Type
narrowT t
 = let	(forallVTs, letVTs, contexts, shape)
 			= stripSchemeT t
		
	depMap		= Map.fromList 
			$ map (\(v, t) -> (v, Set.toList $ freeVarsT t))
			$ letVTs
		
	shapeVs		= freeVarsT shape
	reachVs		= graphReachable depMap
			$ Set.toList 
			$ shapeVs

	t'		= subTLet1 $ narrowTLetsT shapeVs t


  in	{- trace 	( "narrowT\n"
		% "    t       = \n"	%> t			% "\n"
  		% "    shape   = "	% shape			% "\n"
		% "    shapeVs = "	% Set.toList shapeVs	% "\n"
		% "    reachVs = "	% Set.toList reachVs	% "\n\n"
		% "    t'      = \n"	%> t'			% "\n")  -}
		
		t'
		
narrowTLetsT :: Set Var -> Type -> Type
narrowTLetsT    keepVs tt
 = case tt of
 	TForall v t1 t2	-> TForall v t1 (narrowTLetsT keepVs t2)
{-
	TLet    v t1 t2
	 | Set.member v keepVs	-> TLet v t1 (narrowTLetsT keepVs t2)
	 | otherwise		-> narrowTLetsT keepVs t2
-} 
	t		-> t	


-----
-- subTLet1
--	If this type is just a (let v = t in v)  then convert it to t.

subTLet1 ::	Type -> Type
{-
subTLet1	(TLet v1 t1 (TVar k v2))
	| v1 == v2	= t1
-}

subTLet1	t	= t

