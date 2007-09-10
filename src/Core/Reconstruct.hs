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
 = trace 
 	( "addVT: " % v 	% "\n"
 	% "   t = " %> t 	% "\n")
	$ case Map.lookup v tt of
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
   
reconX tt exp@(XAPP x t2)
 = let	(x', tx)	= reconX tt x
   in	case applyTypeT tx t2 of
   	 Just t		-> (XAPP x' t2, t)
	  
	 _ -> panic stage
	 	$ "reconX: Type error in type application.\n"
		% "   exp     =\n" %> exp	% "\n\n"
		% "    x1     =\n" %> x		% "\n\n"
		% "  T[x1]    =\n" %> tx	% "\n\n"
		% "     t     =\n" %> t2	% "\n\n"
	 	
	  
{-   
reconX tt (XTet v t x)
 = let	tt'		= addVT tt (v, t)
 	(x', tx)	= reconX tt' x
   in	( XTet v t x'
   	, TLet v t tx)
 -} 
   
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
	 	$ "reconX: Type error in value application.\n"
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
	
reconX tt xx	= (xx, TNil)


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
{-
applyValueT (TLet v t1 t2) t		
	| Just (t2', eff)	<- applyValueT t2 t
	= Just (TLet v t1 t2', TLet v t1 eff)
-}
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

-----
-- flattenT
--	Flatten out a type, substituting any lets we have for type and region vars.
--	Leave effects and closure as they are though, because their big and they might be graphical.



{-
-----
reconstructTree
	:: Tree		-- header tree 
	-> Tree		-- core tree
	-> Tree		-- core tree with reconstructed types on bindings.

reconstructTree cHeader cTree
 = evalState (walkZM 
 		walkTableId 
			{ transS 	= reconstructS 
			, boundT	= slurpTypeMapPs (cHeader ++ cTree) }
		cTree)
		()	
-----
reconstructS table s
 = case s of
	SBind mV xx
	 -> do	xx'	<- reconstructX table xx
	 	return	$ SBind mV xx'
		
	_ -> return s
	

reconstructX 
	:: WalkTable ReconM
	-> Exp
	-> ReconM Exp
	
reconstructX table xx 
 = case xx of

	-- reconstruct type info anyway so we get the effects as well.
	XTau t x
	 -> do	x'	<- reconstructX table x
	 	case x' of
			XTau t' x2	-> return $ XTau t' x2
			_		-> return xx

	XTet v t x	
	 -> do	x'	<- reconstructX table x
	 	return	x'

	-- lambda's already have types embedded in them.
	XLam{}		-> return xx
	XLAM{}		-> return xx

	-- lookup type of var
	XVar v
	 -> do	let mT	= Map.lookup v (boundT table)
	 	let t'	= case mT of
				Nothing -> panic stage
					$ "reconstructS: not type for " % v
					% "    bound = " % Map.keys (boundT table) % "\n"
				Just t	-> t

	 	return	$ XTau t' xx

	-- reconstruct types of applications.
	XAPP{}		-> reconstructApp table xx
	XApp{}		-> reconstructApp table xx


	_	
	 ->	return xx
	
	

reconstructApp table x
 =  do
	let parts	= flattenAppsE x

 	let (XAppFP (XVar var) Nothing : args)
			= parts
		
	let mT		= Map.lookup var (boundT table)
	let t		= case mT of 
				Just t	-> t
				Nothing	-> panic stage
					$ "reconstructApp: can't find type for " % var % "\n"
		
	let ?appVar	= var
	let ?appType	= t
	let ?appExp	= x

	let (tResult, sub, ee)
			= applyT [] t args []

	let tResultSub	= subBitsT sub tResult
	let eeSub	= map (liftM (subBitsE sub)) ee

	let x'		= unflattenAppsE 
			$ XAppFP (XVar var) Nothing
			: zipWith (fillEffect sub) args eeSub

	return	$ 
--		(Debug.trace $ pretty 
--			$ "reconstructApp\n"
--			% "  mV = " % mV % "\n"
--			% "   x = " %  x % "\n\n")
		(XTau tResultSub x')

fillEffect sub arg mEff
	| Just e		<- mEff
	, XAppFP x _		<- arg
	= XAppFP x (Just e)
	
	
	| otherwise
	= arg
					


-----
-- Perform type applications and work out result type of 
--	an expression.
--
--
-- BUGS: doesn't check type of args
--	 this would be a v.useful sanity check.
--
--	also build effect information
--	also handle fetters on elements of return type.
--

applyT 	:: (?appVar	:: Var)
	-> (?appType 	:: Type)
	-> (?appExp	:: Exp)
	-> [(Var, Type)]				-- type substitutions
	-> Type 					-- type of function.
	-> [Exp]					-- type of arguments to function.
	-> [Maybe Effect]				-- type of arguments to function, with inserted effect information.
	-> (Type, [(Var, Type)], [Maybe Effect])	-- type of result of function.

applyT sub tt xx ee
 = applyT' sub tt xx ee


applyT' sub tt xx ee
	| TLet v t1 t2					<- tt
	= applyT ((v, t1) : sub) t2 xx ee

	-- Forall, bind a type variable.
	--
	| TForall vT (TKind k) t			<- tt
	, XAppFP (XType xT) Nothing : xs		<- xx
	= if kindOfType xT == k
		then applyT ((vT, xT) : sub)
			t xs (Nothing : ee)
	
		else panic stage
			$ "applyT: unexpected type error in core\n"
			% "  tt            = \n " %> tt	% "\n"
			% "  xx            = \n " %> xx	% "\n\n"
			% "\n"
			% "  kind          =  " % k		% "\n"
			% "  kindOfType (" % show xT % ") = "  % kindOfType xT	% "\n"
		
	
	-- Forall, bind a class constraint.
	--
	| TContext c1 t					<- tt
	, XAppFP (XType c2@(TClass v ts)) Nothing : xs	<- xx
	= if matchC2 c1 c2
		then applyT sub 
			t xs (Nothing : ee)
			
		else panic stage
			$ "applyT: unexpected type error in core\n"
			% "  tt             = \n" % tt	% "\n"
			% "  xx             = \n" % xx  % "\n\n"
	
	
	-- A function without effects.
	| TFunEC t1 t2 TPure clo			<- tt
	, XAppFP x@(XVar _) (Just _)	: xs		<- xx
	= applyT
		sub 
		t2
		xs
		(Just TPure : ee)

	
	-- A function with known effects.
	| TFunEC t1 t2 (TVar KEffect eV) clo		<- tt
	, XAppFP x@(XVar _) (Just _) : xs		<- xx
	, Just eff					<- lookup eV sub
	= applyT 
		sub 
		t2 
		xs
		(Just eff : ee)

	-- A function with bound effects.
	| TFunEC t1 t2 e clo			<- tt
	, XAppFP x@(XVar _) (Just _) : xs		<- xx
	= applyT
		sub 
		t2
		xs
		(Just e : ee)


	-- No more arguments, 
	--	return final type.
	| []						<- xx
	= ( tt
	  , sub
	  , reverse ee)


	-- Function has been applied to more arguments than there are function ctors
	--	in the type scheme. This is OK provided the result type is a type var. 
	--
	--	eg  let tuple = ( (\x -> x), 5)
	--	    	y     = fst tuple ();
	--
	--	   where  fst :: forall t0 t1 %r0 !e0 
	--	              .  Tuple %r0 t0 t1 -> t0;
	--
	| TVar k v0					<- tt
	, XAppFP x _ : xs				<- xx
	= case lookup v0 sub of
		Nothing 
		 -> panic stage
		 $  "applyT: can't find type in over-applied expression\n"
	 	 %  "  appVar  = " % ?appVar	% "\n\n"
		 %  "  appType = " % ?appType	% "\n\n"
		 %  "  appExp  = " % ?appExp	% "\n\n"
		 
		Just t0	
		 -> applyT
		    	sub
			t0
			xx
			ee
			
	-- Uh oh.	
	| otherwise
	= panic stage
	$ "applyT: unexpected type-conflict in core. \n"
	% "  appVar  = " % ?appVar % "\n"
	% "\n"

	% "  appType =\n"
	%> (?appType % "\n\n")

	% "  appExp  = " % ?appExp  % "\n\n"
	% "  tt = " % show tt % "\n\n"
	% "  xx = " % show xx % "\n\n"	



	
	
subBitsT 
	:: [(Var, Type)]
	-> Type
	-> Type
	
subBitsT bits tt
	= Trans.transZ 
		Trans.transTableId 
			{ Trans.transT = \t -> return $ subBitsT' bits t }
		tt

subBitsT' bits tt
	| TVar k v		<- tt
	, Just t'		<- lookup v bits
	= t'
	


	| otherwise
	= tt


-----
-- subBitsE
--	Expand out an effect with the provided type substitution.
--	Be careful not to loop forever with recursively defined effects.
--
subBitsE :: [(Var, Type)] -> Effect -> Effect
subBitsE bits ee
	= ee
--	= sumEffs $ expandEs bits ee

expandEs :: [(Var, Type)] -> Effect -> [Effect]
expandEs bits ee
	| TSum KEffect es		<- ee
	= catMap (expandEs bits) es
	
	| TVar KEffect v	<- ee
	, e'	<- lookup v bits
	= catMap (expandEs bits) (crushEffs e' \\ [EVar v])
	
	| ECon v ts		<- ee
	= [ECon v (map (subBitsT bits) ts)]
	
	| otherwise
	= [ee]
	
	 
-----
-- subBitsC
--	Expand out a closure with the provided type substitution.
--	Be careful not to loop forever with recursively defined closures.
--
subBitsC :: [(Var, Type)] -> Closure -> Closure
subBitsC bits cc
	= cc

	= CSum (expandCs bits cc)
	
expandCs :: [(Var, Type)] -> Closure -> [Closure]
expandCs bits cc
	| CSum cs		<- cc
	= catMap (expandCs bits) cs
	
	| CVar v		<- cc
	, Just (TClosure c')	<- lookup v bits
	= catMap (expandCs bits) (crushClo c' \\ [CVar v])
	
	| CFreeT v t		<- cc
	= [CFreeT v (subBitsT bits t)]
	
	| otherwise
	= [cc]
	
-}

