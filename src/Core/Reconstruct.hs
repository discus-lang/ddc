-- Core.Reconstruct
--	Check type information in core
-- 
-- TODO: also check witnesses and proofs of purity.
--	Check typess of bindings.
--

module Core.Reconstruct
	( reconstructTree )

where

import Core.Exp
import Core.Util
import Core.ReconKind
import Core.Plate.FreeVars

import Shared.Error
import Shared.VarPrim
import Util.Graph.Deps
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug


-----
stage	= "Core.Reconstruct"

debug		= True
trace ss x	= if debug then Debug.trace (pretty ss) x else x

reconstructTree
	:: String	-- stage name
	-> Tree		-- header tree 
	-> Tree 	-- core tree
	-> Tree		-- core tree with reconstructed type information
	
reconstructTree stage tHeader tCore
 = let 	?stage	= stage
   in let
 	-- slurp out all the stuff defined at top level
	topTypes	= catMap slurpTypesP (tHeader ++ tCore)
 	tt		= foldr addEqVT' initTable topTypes
	
	-- reconstruct type info on each top level thing
	tCore'		= map (reconP tt) tCore
	
      in tCore'
	



addEqVT :: Var -> Type -> Table -> Table
addEqVT v t tt
 = {- trace 
 	( "addVT: " % v 	% "\n"
 	% "   t = " %> t 	% "\n") -}
	 case Map.lookup v (tableEq tt) of
	 	Nothing	-> tt { tableEq = Map.insert v t (tableEq tt) }
		Just _	-> tt { tableEq = Map.insert v t (Map.delete v (tableEq tt)) }

addEqVT' (v, t) tt
	= addEqVT v t tt


addMoreVT :: Var -> Type -> Table -> Table
addMoreVT v t tt
 = {- trace 
 	( "addVT: " % v 	% "\n"
 	% "   t = " %> t 	% "\n") -}
	 case Map.lookup v (tableMore tt) of
	 	Nothing	-> tt { tableMore = Map.insert v t (tableMore tt) }
		Just _	-> tt { tableMore = Map.insert v t (Map.delete v (tableMore tt)) }

	
-----
-- | A table to carry additional information we collect when decending into the tree
--	Eq 		constraints come from type level lets, and value lambda bindings.
--	More (:>) 	come from constraints on type lambdas bindings.
--
data Table
	= Table
	{ tableEq	:: Map Var Type		-- T[v1] == t2
	, tableMore	:: Map Var Type }	-- T[v1] :> t2

initTable
	= Table
	{ tableEq	= Map.empty
	, tableMore	= Map.empty }

-----
reconP	:: (?stage :: String)
	-> Table -> Top -> Top

reconP tt (PBind v x)
 = let	(x', xt, xe, xc)	
 		= reconX tt x
   in	PBind v x'

reconP tt p		= p

------------------------------------------------------------------------------------------
-- | Reconstruct the type, effect and closure of this expression.
--
reconX 	:: (?stage :: String)	-- name of stage that called this reconstruct
  	=> Table		-- ^ var -> type 
	-> Exp 			-- ^ expression to reconstruct info on
	-> ( Exp		-- expression with reconstructed info
	   , Type		-- type of expression
	   , Type		-- effect of expression
	   , Type)		-- closure of expression

-- LAM
reconX tt (XLAM b@(BMore v t1) t2 x)
 = let	tt'			= addMoreVT v t1 tt
 	(x', xT, xE, xC)	= reconX tt' x
   in	( XLAM b t2 x'
   	, TForall b t2 xT
	, xE
	, xC)

reconX tt (XLAM v k@KClass{} x)
 = let	(x', tx, xe, xc)	= reconX tt x
   in	( XLAM 	   v k x'
    	, TContext k tx 
	, xe
	, xc)

reconX tt (XLAM v k x)
 = let	(x', tx, xe, xc)	= reconX tt x
   in	( XLAM 	  v k x'
    	, TForall v k tx 
	, xe
	, xc)
 
-- APP
reconX tt exp@(XAPP x t)
 = let	(x', tx, xe, xc)	= reconX tt x
   in	case applyTypeT tt tx t of
   	 Just t'	
	  -> 	( XAPP x' t
	  	, t'
		, xe
		, xc)
	  
	 _ -> panic stage
	 	$ " reconX: Kind error in type application (x t).\n"
		% "  in stage " % ?stage	% "\n\n"
		% "     x     =\n" %> x		% "\n\n"
		% "     t     =\n" %> t		% "\n\n"
		% "   T[x]    =\n" %> tx	% "\n\n"
-- tet
reconX tt (XTet vts x)
 = let	tt'			= foldr addEqVT' tt vts
 	(x', tx, xe, xc)	= reconX tt' x
   in	( XTet   vts x'
   	, TWhere tx vts
	, xe
	, xc)
   
-- xtau
-- We can't actually check the reconstructed type against the annotation here
--	because we can't see /\ bound TREC variables that might be bound above us.
--
--	eg, with /\ t -> [** t] x
--	    the type of x is (forall t. t), not just t.
--
-- 	The XTau types are checked by reconS instead.
--
reconX tt exp@(XTau tauT x)
 = let	(x', xT, xE, xC)	= reconX tt x
   in	( XTau tauT x'
	, xT
	, xE
	, xC)

{-

 	tauT'			= flattenT tauT
	xT'			= packT $ substituteT (tableEq tt) xT
   in	if subsumes (tableMore tt) tauT' xT'
    	 then	( XTau tauT x'
	   	, xT
		, xE
		, xC)

	 else	panic stage
	 		$ "reconX: Type error in core.\n"
			% " in annotated expression:\n"			%> exp		% "\n\n"
			% " reconstructed type:\n"			%> xT'		% "\n\n"
			% " is not less than type of annotation:\n"	%> tauT'	% "\n\n"
-}

-- lam
reconX tt exp@(XLam v t x eff clo)
 	| tt'			<- addEqVT v t tt
	, (x', xT, xE, xC)	<- reconX tt' x

	, eff'			<- packT $ substituteT (tableEq tt) eff
	, clo'			<- packT $ substituteT (tableEq tt) clo
	, xC'			<- trimClosureC $ makeTMask KClosure xC (TTag v)
	, xE'			<- packT xE
	
	-- check effects match
	, () <- if subsumes (tableMore tt) eff' xE'
		 then ()
		 else panic stage
			$ "reconX: Effect error in core.\n"
			% " in lambda abstraction:\n" 			%> exp	% "\n\n"
			% " reconstructed effect of body:\n" 		%> xE'	% "\n\n"
			% " does not match effect annot on lambda:\n"	%> eff'	% "\n\n"

	-- check closures match
	, () <- if subsumes (tableMore tt) clo' xC'
		 then ()
		 else panic stage
			$ "reconX: Closure error in core.\n"
			% " in lambda abstraction:\n" 			%> exp	% "\n\n"
			% " reconstructed closure of body:\n" 		%> xC'	% "\n\n"
			% " does not match closure annot on lambda:\n"	%> clo'	% "\n\n"
	

	= ( XLam v t x' eff clo
	  , TFunEC t xT eff clo
	  , TBot KEffect
	  , xC')

-- local
reconX tt (XLocal v vs x)
 = let	(x', xT, xE, xC)	= reconX tt x
   in	( XLocal v vs x'
   	, xT
	, makeTSum
		KEffect
		(map 	(\e -> case e of
				TEffect vE [TVar KRegion r]
				 |   elem vE [primRead, primWrite]
				  && r == v
				 -> TBot KEffect
				 
				_	-> e)
			$ flattenTSum xE)
	, xC)
	
-- app
reconX tt exp@(XApp x1 x2 eff)
 = let	(x1', x1t, x1e, x1c)	= reconX tt x1
	(x2', x2t, x2e, x2c)	= reconX tt x2
	mResultTE		= applyValueT tt x1t x2t
   in	case mResultTE of
   	 Just (appT, appE)
  	  -> let x'		= XApp x1' x2' (packT appE)
     	     in ( x'
	        , appT
		, makeTSum KEffect  [x1e, x2e, appE]
		, makeTSum KClosure [x1c, x2c])
	       	
	 _ -> panic stage	
	 	$ "reconX: Type error in value application (x1 x2).\n"
		% " in stage: " % ?stage			% "\n"
		% " in expression:\n"
		% "     (" % x1 % ") " % "(" % x2	% ")" % "\n\n"

		% "   T[x1]   = " %> x1t		% "\n\n"
		% "   (flat)  = " %> flattenT x1t	% "\n\n"
		

		% "   T[x2]   = " % x2t		% "\n\n"
   
-- do
reconX tt (XDo ss)
 = let	(tt', sts)		= mapAccumL reconS tt ss
	(ss', sTs, sEs, sCs)	= unzip4 sts
	Just t			= takeLast sTs
	vsBind			= catMaybes $ map takeVarOfStmt ss'
	
   in	( XDo ss'
        , t
	, makeTSum KEffect sEs
	, makeTMask 
		KClosure
		(makeTSum KClosure sCs)
		(makeTSum KClosure (map TTag vsBind)) )
   
-- match
reconX tt (XMatch aa eff)
 = let	(aa', altTs, altEs, altCs)	= unzip4 $ map (reconA tt) aa
 	Just atLast			= takeLast altTs
   in	( XMatch aa' eff
   	, atLast
	, makeTSum KEffect altEs
	, makeTSum KClosure altCs )


-- const	
reconX tt (XConst c t)
 =	( XConst c t
 	, t
	, TBot KEffect
	, TBot KClosure )
	

-- var
reconX tt (XVar v)
 = case Map.lookup v (tableEq tt) of
 	Just t		
	 -> let	t'	= inlineTWheresMapT (tableEq tt) Set.empty t
	    in  ( XVar v
	        , t'
		, TBot KEffect
		, TFree v t)
	
	Nothing		
	 -> panic stage $ "reconX: Variable " % v % " is not bound"

-- prim
reconX tt xx@(XPrim prim xs eff)
 = let	(xs', xsTs, xsEs, xsCs)		
 		= unzip4 $ map (reconX tt) xs
		
	Just t	= maybeSlurpTypeX xx
   in	( xx
   	, t
   	, makeTSum KEffect  (eff : xsEs)
	, makeTSum KClosure xsEs)

-- no match
reconX tt xx
 	= panic stage 
 	$ "reconX: no match for " ++ show xx



-----
reconS 	:: (?stage :: String) 
	=>  Table -> Stmt -> (Table, (Stmt, Type, Effect, Closure))

reconS tt (SBind Nothing x)	
 = let	(x', xt, xe, xc)	= reconX tt x
   in	( tt
   	, ( SBind Nothing (dropXTau x' Map.empty (packT xt) )
	  , xt
	  , xe
	  , xc))

reconS tt (SBind (Just v) x)
 = let	(x', xT, xE, xC)	= reconX tt x
	tt'			= addEqVT v xT tt
   in	( tt'
   	, ( SBind (Just v) (dropXTau x' Map.empty (packT xT))
	  , xT
	  , xE
	  , TMask KClosure xC (TTag v)))
	  

-----
reconA 	:: (?stage :: String)
	=> Table -> Alt -> (Alt, Type, Effect, Closure)

reconA tt (AAlt gs x)
 = let	(tt', gecs)		= mapAccumL reconG tt gs
	(gs', vssBind, gEs, gCs)= unzip4 gecs
	(x', xT, xE, xC)	= reconX tt' x
   in	( AAlt gs' x'
   	, xT
	, makeTSum KEffect  (gEs ++ [xE])
	, makeTMask 
		KClosure
		(makeTSum 
			KClosure 
			(gCs ++ [xC]))
		(makeTSum
			KClosure
			(map TTag $ concat vssBind)))
  
 
-----
-- BUGS: check type of pattern agains type of expression
--
reconG	:: (?stage :: String)
	=> Table -> Guard -> (Table, (Guard, [Var], Effect, Closure))

reconG tt (GExp p x)
 = let	binds		= slurpVarTypesW p
 	tt'		= foldr addEqVT' tt binds
	(x', xT, xE, xC)= reconX tt' x
	
	-- Work out the effect of testing the case object.
	eff		= case xT of
			   TData vD []	
			     -> TBot KEffect

			   TData vD (TVar KRegion rH : _)
			     -> TEffect primRead [TVar KRegion rH]
   in	( tt'
   	, ( GExp p x'
	  , map fst binds
   	  , makeTSum KRegion ([xE, eff])
	  , xC))
 

slurpVarTypesW (WConst{})	= []
slurpVarTypesW (WCon v lvt)	= map (\(l, v, t)	-> (v, t)) lvt


-- | Work out the result type and latent effect that will result when 
--	an arg is applied to a function with this type.
--
applyValueT 
	:: Table		-- ^ table of constraints
	-> Type 		-- ^ type of function
	-> Type 		-- ^ type of arg
	-> Maybe 		
		( Type		-- result type
		, Effect)	-- effect caused

applyValueT table t1 t2
 =	applyValueT' table (flattenT t1) (flattenT t2)
 
applyValueT' table t0@(TFunEC t1 t2 eff clo) t3	
	= if subsumes (tableMore table) t1 t3
		then Just (t2, eff)
		else freakout stage
			( "applyValueT: Type error in value application.\n"
			% "    can't apply argument:\n"	%> t3 % "\n\n"
			% "    to:\n"        		%> t0 % "\n"
			% "\n"
			% "    as it is not <: than:\n"	%> t1 % "\n"
			% "\n"
			% "    with bounds: \n"
			% ("\n" %!% (map (\(v, b) -> "        " % v % " :> " % b) 
				$ Map.toList (tableMore table)) % "\n\n"))
			$ Nothing
	
applyValueT' _ _ _
	= Nothing
	
	
-----
-- applyTypeT
--	Apply a value argument to a forall/context type, yielding the result type.
--	BUGS: check that the kinds/contexts match as we apply.
--
applyTypeT :: Table -> Type -> Type -> Maybe Type

applyTypeT table (TForall (BVar v) k t1) t2
	= Just (substituteT (Map.insert v t2 Map.empty) t1)

applyTypeT table (TForall (BMore v tB) k t1) t2
	-- check that the constraint is satisfied
	| subsumes (tableMore table) tB t2
	= Just (substituteT (Map.insert v t2 Map.empty) t1)
	

applyTypeT table t1@(TContext k11 t12) t2
	-- witnesses must match
	| packK k11 == packK (kindOfType t2)
	= Just t12

	| otherwise
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    can't apply\n"		%> t2	% "\n\n"
		% "    to\n"			%> t1	% "\n\n"
		% "    k11\n"		%> (packK k11)	% "\n\n"
		% "    K[t2]\n"		%> (packK (kindOfType t2))	% "\n\n")
		$ Nothing

applyTypeT table (TWhere t1 vts) t
	| Just t1'	<- applyTypeT table t1 t
	= Just $ TWhere t1' vts
	
applyTypeT table t1 t2
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    can't apply\n"		%> t2	% "\n\n"
		% "    to\n"			%> t1	% "\n\n")
		$ Nothing



