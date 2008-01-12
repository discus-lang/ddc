-- Core.Reconstruct
--
--	Reconstruct and check the type/region/effect/closure and witness information in
--	the core IR.
--
-- 	The types of free variables can be supplied either as annotations on the variables themselves, 
--	or supplied via a table. Any missing annotations are filled during checking, so expressions
--	returned may be checked again without using the table.
--	
--	The table also carries the name of the calling function, to print in panic messages incase
--	a type error is uncoverred.
--
--	The prime versions of the recon* functions start with a stage name and an empty table, but are 
--	otherwise identical to the plain versions.
--
--	The recon?_type versions take a stage name and only return the value type of the expression.
--	
-- 	TODO: also check witnesses and proofs of purity.
--
--

module Core.Reconstruct
	( reconTree
	, reconP, reconP', reconP_type
	, reconX, reconX', reconX_type
	, reconS
	, reconA
	, reconG

	, Table (..)
	, emptyTable
	, addEqVT
	, addMoreVT)
where

import Core.Exp
import Core.Util
import Core.ReconKind
import Core.Plate.FreeVars

import Shared.Error
import Shared.VarPrim
import Util.Graph.Deps
import Util

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Debug.Trace	as Debug


-----
stage	= "Core.Reconstruct"

debug		= False
trace ss x	= if debug then Debug.trace (pretty ss) x else x


-- Tree --------------------------------------------------------------------------------------------
reconTree
	:: String	-- caller name
	-> Tree		-- header tree 
	-> Tree 	-- core tree
	-> Tree		-- core tree with reconstructed type information
	
reconTree caller tHeader tCore
 = {-# SCC "reconstructTree" #-}
   let
	table		= emptyTable { tableCaller = Just caller }

 	-- slurp out all the stuff defined at top level
	topTypes	= {-# SCC "reconTree/topTypes" #-} catMap slurpTypesP (tHeader ++ tCore)
 	tt		= {-# SCC "reconTree/table"    #-} foldr addEqVT' table topTypes
	
	-- reconstruct type info on each top level thing
	tCore'		= map (reconP tt) tCore
	
      in tCore'
	

-- Top ---------------------------------------------------------------------------------------------
reconP' :: String -> Top -> (Top, Type, Effect, Closure)
reconP' caller (PBind v x)

 = let	(SBind (Just v') x', typ', eff', clo')
 		= snd 
		$ reconS emptyTable { tableCaller = Just caller } 
		$ SBind (Just v) x
		
   in	(PBind v' x', typ', eff', clo')
 

reconP_type :: String -> Top -> Type
reconP_type caller p
	= t4_2 $ reconP' caller p


reconP	:: Table 
	-> Top 
	-> Top

reconP tt (PBind v x)
 = let	(x', xt, xe, xc)	
 		= {-# SCC "reconP/reconX" #-} reconX tt x
   in	PBind v x'

reconP tt p		= p


-- Expression --------------------------------------------------------------------------------------
reconX' :: String -> Exp -> (Exp, Type, Effect, Closure)
reconX' caller x 
	= reconX emptyTable { tableCaller = Just caller } x

reconX_type :: String -> Exp -> Type
reconX_type caller x
	= t4_2 $ reconX' caller x

reconX 	:: Table
	-> Exp 
	-> ( Exp
	   , Type, Effect, Closure)

-- LAM
reconX tt xx@(XLAM b@(BMore v t1) t2 x)
 = trace ("reconX[XLam]: " % xx % "\n")
 $ let	tt'			= addMoreVT v t1 tt
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
		% "     caller = "  % tableCaller tt	% "\n"
		% "     x      =\n" %> x		% "\n\n"
		% "     t      =\n" %> t		% "\n\n"
		% "   T[x]     =\n" %> tx	% "\n\n"

-- Tet
reconX tt (XTet vts x)
 = let	tt'			= foldr addEqVT' tt vts
 	(x', tx, xe, xc)	= reconX tt' x
   in	( XTet   vts x'
   	, TFetters tx (map (uncurry FWhere) vts)
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
 = {-# SCC "reconX/XLam" #-}
   let	reconX_lam
 
	 	| tt'			<- addEqVT v t tt
		, (x', xT, xE, xC)	<- reconX tt' x

		, eff'		<- packT $ substituteT (tableEq tt) eff
		, clo_sub	<- packT $ substituteT (tableEq tt) clo

		-- TODO: We need to flatten the closure before trimming to make sure effect annots
		--	on type constructors are not lost. It would be better to modify trimClosureC
		--	so it doesn't loose them, or the closure equivalence rule so it doesn't care.
		, xC_masked	<- makeTMask KClosure xC (TTag v)
		, xC_flat	<- flattenT xC_masked
		, xC'		<- trimClosureC $ xC_flat

		, xE'		<- packT xE
	
		-- check effects match
		, () <- if subsumes (tableMore tt) eff' xE'
			 then ()
			 else panic stage
				$ "reconX: Effect error in core.\n"
				% "    caller = " % tableCaller tt			% "\n"
				% "    in lambda abstraction:\n" 			%> exp	% "\n\n"
				% "    reconstructed effect of body:\n" 		%> xE'	% "\n\n"
				% "    does not match effect annot on lambda:\n"	%> eff'	% "\n\n"

		-- check closures match
		, () <- if subsumes (tableMore tt) clo_sub xC'
			 then ()
			 else panic stage
				$ "reconX: Closure error in core.\n"
				% "    caller = " % tableCaller tt			% "\n"
				% "    in lambda abstraction:\n" 			%> exp		% "\n\n"
				% "    reconstructed closure of body:\n" 		%> xC'		% "\n\n"
				% "    does not match closure annot on lambda:\n"	%> clo_sub	% "\n\n"
	

		= ( XLam v t x' eff clo
		  , TFunEC t xT eff clo
		  , TBot KEffect
		  , xC')

   in	reconX_lam

-- local
-- TODO: check well foundedness of witnesses

reconX tt (XLocal v vs x)
 = let	(x', xT, xE, xC)	= reconX tt x

   in	(if False -- Set.member v (freeVars xT) 
   		then panic stage 
			( "reconX: region " % v % " is not local\n"
			% "    caller = " % tableCaller tt	% "\n"
			% "    t      = " % xT	% "\n"
			% "    x      = " % x'	% "\n\n")
			id
		else	id)
	 
   	( XLocal v vs x'
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
	mResultTE		= {-# SCC "reconX/applyValue" #-}
	                          applyValueT tt x1t x2t
   in	case mResultTE of
   	 Just (appT, appE)
  	  -> let x'		= XApp x1' x2' pure
     	     in ( x'
	        , appT
		, makeTSum KEffect  [x1e, x2e, appE]
		, makeTSum KClosure [x1c, x2c])
	       	
	 _ -> panic stage	
	 	$ "reconX: Type error in value application (x1 x2).\n"
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
	, trimClosureC 
		$ makeTMask 
			KClosure
			(makeTSum KClosure (map (trimClosureC . flattenT) sCs))
			(makeTSum KClosure (map TTag vsBind)) )
   
-- match
reconX tt (XMatch [])
 = panic stage
 	$ pretty "reconX: XMatch has no alternatives\n"

reconX tt (XMatch aa)
 = let	(aa', altTs, altEs, altCs)	= unzip4 $ map (reconA tt) aa
 	Just atLast			= takeLast altTs
   in	( XMatch aa'
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
-- TODO: check against existing annotation.


-- var has no type annotation, so look it up from the table
reconX tt (XVar v TNil)
	| Just t	<- Map.lookup v (tableEq tt)
	, t'		<- inlineTWheresMapT (tableEq tt) Set.empty t 

	-- When we add the type to this var we need to attach any more constraints associated with it, 
	--	else we won't be able to check expressions separate from their enclosing XLAMs 
	--	(which carry these constraints)
	, vsFree	<- freeVars t
	, vtsMore	<- catMaybes
			$  map (\u -> case Map.lookup u (tableMore tt) of
						Nothing	-> Nothing
						Just t	-> Just (u, t))
			$ Set.toList vsFree

	, tDrop		<- makeTFetters t' (map (uncurry FMore) vtsMore)

	= trace ( "reconX[XVar]: dropping type\n"
		% "    var    = " %> v		% "\n"
		% "    tDrop  = " %> tDrop	% "\n")
	   $	
	
	    ( XVar v tDrop
	    , tDrop
	    , TBot KEffect
	    , TFree v t)
	  
	| otherwise
	= panic stage 
	 	$ "reconX: Variable " % v % " has no embeded type annotation and is not in the provided environment.\n"
		% "    caller = " % tableCaller tt	% "\n"

	
-- var has a type annotation, so use that as its type
reconX tt (XVar v t)
 = let	t'	= inlineTWheresMapT (tableEq tt) Set.empty t
   in	( XVar v t
	, t'
	, TBot KEffect
	, TFree v t)


-- prim
reconX tt xx@(XPrim prim xs)
 = trace ("reconX[XPrim]: " % xx % "\n")
 $ let	
 	-- some of the xs are type terms which we can't call recon on
	--	so we have to do some contortions to get the closure from the others.
	reconMaybeX tt x
	 = case x of
	 	XType{}	-> (x, Nothing, Nothing, Nothing)
		_	->
		 let (x', typ, eff, clo)	= reconX tt x
		 in  (x', Just typ, Just eff, Just clo)
		 
 
  	(xs', _, xsmEs, xsmCs)		
 		= unzip4 $ map (reconMaybeX tt) xs

	t	= case prim of
			MBox   tRes _	-> tRes
			MUnbox tRes _	-> tRes
			MTailCall	-> reconApps tt xs'
			MCall		-> reconApps tt xs'
			MCallApp _	-> reconApps tt xs'
			MApply		-> reconApps tt xs'
			MCurry _	-> reconApps tt xs'
			MFun		-> reconApps tt xs'

			_		-> panic stage
					$ "reconX[Prim]: no match for " % prim % "\n"
		
   in	( XPrim prim xs'
   	, t
   	, makeTSum KEffect  $ catMaybes xsmEs
	, makeTSum KClosure $ catMaybes xsmCs)




-- no match
reconX tt xx
 	= panic stage 
 	$ "reconX: no match for " % show xx	% "\n"
	% "    caller = " % tableCaller tt	% "\n"


-- | Reconstruct the result type when this list of expressions
--	is applied in a left to right order.
--
--	eg  [x1, x2, x3, x4] =>  ((x1 x2) x3) x4
--
reconApps 
	:: Table
	-> [Exp] 
	-> Type

reconApps table [XType t]
 =	t
 
reconApps table [x]
 = 	t4_2 $ reconX table x

reconApps table (XType t1 : XType t2 : xs)
 = let	Just t12	= applyTypeT table t1 t2
   in	reconApps table (XType t12 : xs)
 	
reconApps table (x1 : XType t2 : xs)
 = let	t1		= t4_2 $ reconX table x1
 	Just t12	= applyTypeT table t1 t2
   in	reconApps table (XType t12 : xs)

reconApps table (XType t1 : x2 : xs)
 = let	t2		= t4_2 $ reconX table x2
 	Just (t12, _)	= applyValueT table t1 t2
   in	reconApps table (XType t12 : xs)

reconApps table (x1 : x2 : xs)
 = let	t1		= t4_2 $ reconX table x1
	t2		= t4_2 $ reconX table x2
 	Just (t12, _)	= applyValueT table t1 t2
   in	reconApps table (XType t12 : xs)


-- Stmt --------------------------------------------------------------------------------------------

-- | running reconS also adds a type for this binding into the table
reconS 	:: Table 
	-> Stmt 
	-> (Table, (Stmt, Type, Effect, Closure))

reconS tt (SBind Nothing x)	
 = let	(x', xt, xe, xc)	= reconX tt x
   in	( tt
   	, ( SBind Nothing x'
	  , xt
	  , xe
	  , xc))

reconS tt (SBind (Just v) x)
 = let	(x', xT, xE, xC)	= reconX tt x
	tt'			= addEqVT v xT tt
   in	( tt'
   	, ( SBind (Just v) x'
	  , xT
	  , xE
	  , TMask KClosure xC (TTag v)))
	  

-- Alt ---------------------------------------------------------------------------------------------
reconA 	:: Table 
	-> Alt 
	-> (Alt, Type, Effect, Closure)

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
  
 
-- Guards ------------------------------------------------------------------------------------------

-- | running reconG also adds a types for the matched variables into the table.

-- TODO: check type of pattern against type of expression
--
reconG	:: Table 
	-> Guard 
	-> ( Table
	   , (Guard, [Var], Effect, Closure))

reconG tt (GExp p x)
 = let	binds		= slurpVarTypesW p
 	tt'		= foldr addEqVT' tt binds
	(x', xT, xE, xC)= reconX tt' x
	
	-- Work out the effect of testing the case object.
	eff		= case stripToShapeT xT of
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




-- Value / Type application functions ---------------------------------------------------------


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

applyValueT' table t1@(TFetters t1Shape fs) t2
 = let	([[], fsMore], [])
 		= partitionFs [(=@=) FWhere{}, (=@=) FMore{}] fs
	
	table'	= foldr addMoreF table fsMore
	
   in	applyValueT' table' t1Shape t2
 
applyValueT' table t0@(TFunEC t1 t2 eff clo) t3	
	= if subsumes (tableMore table) t1 t3
		then Just (t2, eff)
		else freakout stage
			( "applyValueT: Type error in value application.\n"
			% "    called by = " % tableCaller table	% "\n\n"
			% "    can't apply argument:\n"	%> t3 % "\n\n"
			% "    to:\n"        		%> t0 % "\n"
			% "\n"
			% "    as it is not <: than:\n"	%> t1 % "\n"
			% "\n"
			% "    with bounds: \n"
			% ("\n" %!% (map (\(v, b) -> "        " % v % " :> " % b) 
				$ Map.toList (tableMore table)) % "\n\n"))
			$ Nothing
	
applyValueT' _ t1 t2
	= freakout stage
		( "applyValueT: No match for (t1 t2).\n"
		% "    t1 = " % t1	% "\n"
		% "    t2 = " % t2	% "\n")
		Nothing
	
	
-- | Apply a value argument to a forall/context type, yielding the result type.
--	TODO: check that the kinds/contexts match as we apply.
--
applyTypeT :: Table -> Type -> Type -> Maybe Type

applyTypeT table (TForall (BVar v) k t1) t2
	| k == kindOfType t2
	= Just (substituteT (Map.insert v t2 Map.empty) t1)

applyTypeT table (TForall (BMore v tB) k t1) t2
	-- if the constraint is a closure then trim it first
	| k == KClosure
	, subsumes (tableMore table) (flattenT $ trimClosureC t2) (flattenT $ trimClosureC tB)
	= Just (substituteT (Map.insert v t2 Map.empty) t1)

	-- check that the constraint is satisfied
	| subsumes (tableMore table) t2 tB
	= Just (substituteT (Map.insert v t2 Map.empty) t1)
	
	| otherwise
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % tableCaller table % "\n"
		% "   (\\/ " % v % " :> (" % tB % ") " % k % " -> ...)" % " (" % t2 % ")"
		% "\n"
		%> t2 % "\n"
		% "is not :>\n"
		%> tB % "\n"
		% "\n")

		$ Nothing
	

applyTypeT table t1@(TContext k11 t12) t2
	-- witnesses must match
	| packK k11 == packK (kindOfType t2)
	= Just t12

	| otherwise
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % tableCaller table	% "\n"
		% "    can't apply\n"		%> t2	% "\n\n"
		% "    to\n"			%> t1	% "\n\n"
		% "    k11\n"		%> (packK k11)	% "\n\n"
		% "    K[t2]\n"		%> (packK (kindOfType t2))	% "\n\n")
		$ Nothing

applyTypeT table (TFetters t1 fs) t
	| Just t1'	<- applyTypeT table t1 t
	= Just $ TFetters t1' fs
	
applyTypeT table t1 t2
	= freakout stage
		( "applyTypeT: Kind error in type application.\n"
		% "    caller = " % tableCaller table	% "\n"
		% "    can't apply\n"		%> t2	% "\n\n"
		% "    to\n"			%> t1	% "\n\n")
		$ Nothing



-- Table -------------------------------------------------------------------------------------

-- | A table to carry additional information we collect when decending into the tree
--	Eq 		constraints come from type level lets, and value lambda bindings.
--	More (:>) 	come from constraints on type lambdas bindings.
--
data Table
	= Table
	{ -- the name of the function that called this reconstruct.
	  -- this is printed in panic messages.
	  tableCaller	:: Maybe String

	, tableEq	:: Map Var Type		-- T[v1] == t2

	, tableMore	:: Map Var Type }	-- T[v1] :> t2

emptyTable
	= Table
	{ tableCaller	= Nothing
	, tableEq	= Map.empty
	, tableMore	= Map.empty }




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


addMoreF :: Fetter -> Table -> Table
addMoreF (FMore v t) table	= addMoreVT v t table

addMoreVT :: Var -> Type -> Table -> Table
addMoreVT v t tt
 = {- trace 
 	( "addVT: " % v 	% "\n"
 	% "   t = " %> t 	% "\n") -}
	 case Map.lookup v (tableMore tt) of
	 	Nothing	-> tt { tableMore = Map.insert v t (tableMore tt) }
		Just _	-> tt { tableMore = Map.insert v t (Map.delete v (tableMore tt)) }

	
