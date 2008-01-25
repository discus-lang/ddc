
-- Core.Prim
--	Find direct uses of primitive functions and replace them by XPrim nodes.
--	When we walk down the tree we remember what regions are marked as Direct.
--	Direct regions don't need to be forced before being unboxed.
--
module Core.Prim
	( primTree )


where

import Util
import Core.Exp
import Core.Util
import qualified Core.Reconstruct	as Recon
import Core.ReconKind
import Core.Plate.Trans


import Data.Map			(Map)
import qualified Data.Map	as Map

import Data.Set			(Set)
import qualified Data.Set	as Set

import qualified Shared.Var	as Var
import Shared.VarPrim

import qualified Debug.Trace

debug	= False
trace ss x
 = if debug 
 	then Debug.Trace.trace (pprStr ss) x
  	else x

-----
stage	= "Core.Prim"

-- Table -------------------------------------------------------------------------------------------
data Table
	= Table
	{ tableDirectRegions	:: Set Var }
	
tableZero 
	= Table
	{ tableDirectRegions	= Set.empty }


-- | If this is a witness to constness of a region or type, or purity of an effect
--	then slurp it into the table.
slurpWitnessKind 
	:: Table -> Kind -> Table
	
slurpWitnessKind tt kk
 = case kk of
	-- const regions
 	KClass v [TVar KRegion r]
	 |  v == primDirect
	 -> tt { tableDirectRegions 
	 		= Set.insert r (tableDirectRegions tt)}

	_ -> tt

-- Prim --------------------------------------------------------------------------------------------

-- Identify primitive operations
--	Tree should be in snipped form
primTree :: Tree -> Tree
primTree tree
	= snd $ mapAccumL primP tableZero tree

-- top
primP :: Table -> Top -> (Table, Top)
primP tt pp
 = case pp of
 	PBind mV x	
	 -> let	(tt2, x')	= primX tt x
	    in	(tt2, PBind mV x')

	_ -> 	(tt, pp)
	    
-- exp..
--	this boilerplate is mostly pasted from Core.Float .. abstract this somehow?
primX :: Table -> Exp -> (Table, Exp)
primX tt xx
 = case xx of
	-- check for direct regions on the way down
 	XLAM b k x	
	 -> let tt2		= slurpWitnessKind tt k
		(tt3, x')	= primX tt2 x
	    in	(tt3, XLAM b k x')

	-- check for direct regions on the way down
	XLocal v vts x
	 -> let tt2	= foldl' slurpWitnessKind tt 
	 		$ map (kindOfType . snd) vts
		
		(tt3, x')	= primX tt2 x
	    in	(tt3, XLocal v vts x')

	XLam v t x eff clo	
	 -> let (tt', x')	= primX tt x
	    in  (tt', XLam v t x' eff clo)

	XVar{}			-> (tt, xx)
	
	XDo ss
	 -> let	(tt', ss')	= mapAccumL primS tt ss
	    in  (tt', XDo ss')

	XMatch alts		
	 -> let	(tt', alts')	= mapAccumL primA tt alts
	    in  (tt', XMatch alts')
	    
	XAPP x t
	 -> let (tt', x')	= primX tt x
	    in	(tt', XAPP x' t)

	XTet vts x
	 -> let (tt', x')	= primX tt x
	    in  (tt', XTet vts x')
 
	XTau t x
	 -> let (tt', x')	= primX tt x
	    in	(tt', XTau t x')

	XApp x1 x2 eff
	 -> let (tt2, x1')	= primX tt  x1
	        (tt3, x2')	= primX tt2 x2
	    in	(tt3, XApp x1' x2' eff)

	XPrim p xs
	 -> let (tt', xs')	= mapAccumL primX tt xs
	    in	(tt', XPrim p xs')

	XLit{}			-> (tt, xx)
	XType{}			-> (tt, xx)


primA :: Table -> Alt -> (Table, Alt)
primA tt (AAlt gs x)
 = let	(tt2, gs')	= mapAccumL primG tt gs
 	(tt3, x')	= primX tt2 x
   in	(tt3, AAlt gs' x')

primG :: Table -> Guard -> (Table, Guard)
primG tt (GExp ws x)
 = let 	(tt', x')	= primX tt x
   in	(tt', GExp ws x')


-- Identify a primitive operation in this statement
--	We go via statements in snipped form so its easy to identify the entire application expression

--	TODO: 	fix this for over-application
--		need to split off some of the args

primS :: Table -> Stmt -> (Table, Stmt)
primS tt ss
 = case ss of
	-- enter into XTau
	SBind mV (XTau t x)	
	 -> let x2		= primX1 tt x
		(tt3, x3)	= primX tt x2
	    in	(tt3, SBind mV (XTau t x3))
	 
 	SBind mV x		
	 -> let x2		= primX1 tt x
		(tt3, x3)	= primX tt x2

	    in  (tt3, SBind mV x3)

-- do the flat rewrite
primX1 tt xx
	-- direct use of boxing function
	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, isBoxFunctionV v
	= XPrim MBox psArgs

	-- direct use of unboxing function
	| Just parts				<- flattenAppsEff xx
	, XVar v t : psArgs@[XType tR@(TVar KRegion vR), x]
			<- parts
						
	, isUnboxFunctionV v

	= if Set.member vR (tableDirectRegions tt) 
		then XPrim MUnbox psArgs

		-- have to force non-direct objects before unboxing them
		else XPrim MUnbox [XType tR, XPrim MForce [x]]


	-- look for functions in the prim table
 	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, Just operator				<- Map.lookup (Var.name v) primFuns
	= XPrim (MOp operator) psArgs
	
	-- look for functions who's arguments can be unboxed
 	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, Just (operator, actions)		<- Map.lookup (Var.name v) unboxableFuns
	, tResult@(TData vResult tsResult)	<- Recon.reconX_type (stage ++ ".primX") xx
	, length psArgs == length actions
	= trace ( "primX:\n"
		% "    xx      = " % xx 	% "\n"
		% "    parts   = " % parts	% "\n"
		% "    tResult = " % tResult	% "\n") 
	   $ let	
		-- apply the actions to the arguments
		psArgs'	= doActions tt actions psArgs
		
		-- generate the primitive 
	   in	XPrim MBox (map XType tsResult ++ [XPrim (MOp operator) psArgs'])
	
	| otherwise		
	= xx

-- | Perform these actions on this list of expressions
doActions :: Table -> [Action] -> [Exp] -> [Exp]
doActions tt (Discard:as) (x:xs)
	= doActions tt as xs

doActions tt (Ignore:as) (x:xs)
	= x : doActions tt as xs
	
doActions tt (Unbox:as) (x:xs)

 = let	tX@(TData v [tR@(TVar KRegion vR)])	
 		= Recon.reconX_type (stage ++ "doActions") x

	x'	| Set.member vR (tableDirectRegions tt) 
		= XPrim MUnbox [XType tR, x] 
			: doActions tt as xs
		
		| otherwise
		= XPrim MUnbox [XType tR, XPrim MForce [x]] 
			: doActions tt as xs

   in	trace	( "doActions: Unbox\n"
		% "    x    = " % x 	% "\n"
		% "    tX   = " % tX	% "\n")
		$ x'

doActions _ _ xs
	= xs

-- | Flatten an application into its component parts, left to right
flattenAppsEff :: Exp -> Maybe [Exp]
flattenAppsEff xx
 = case xx of
 	XApp{}	-> Just $ flattenAppsEff' xx
	XAPP{}	-> Just $ flattenAppsEff' xx
	_	-> Nothing

flattenAppsEff' xx
 = let	bits	= flattenAppsE xx
 	
	(parts, effss)
		= unzip
		$ map (\p -> case p of
				XAppFP x mEff	-> (x, mEff))
		$ bits
    in	parts
	

-- whether to ignore or unbox the argument
data Action
	= Ignore
	| Discard
	| Unbox
	deriving (Show, Eq)

-- poor man's type signatures
--	Says what to do with the arguments of a function.
--	We tend to ignore type arguments, and unbox boxed values.

-- opI3U2	= [Ignore, Ignore, Ignore, Unbox, Unbox]
opD1U1	= [Discard, Discard, Unbox]
opD3U2	= [Discard, Discard, Discard, Unbox, Unbox]

----------------------------------------------------------------------------------------------------
-- TODO: the code for these tables could be a lot cleverer
--	explicitly listing each member won't be the right way when we want
--	to support all the other primitive types as well.


-- functions who's arguments can be unboxed
unboxableFuns :: Map String (Op, [Action])
unboxableFuns
 = Map.fromList

	-- boxed int functions
	[ ("primInt32_neg",	(OpNeg, opD1U1))
	, ("primInt32_add",	(OpAdd,	opD3U2))
	, ("primInt32_sub",	(OpSub,	opD3U2))
	, ("primInt32_div",	(OpDiv,	opD3U2))
	, ("primInt32_mod",	(OpMod,	opD3U2))
	, ("primInt32_mul",	(OpMul,	opD3U2))

	-- boxed comparisions
	, ("primInt32_eq",	(OpEq,	opD3U2))
	, ("primInt32_neq",	(OpNeq,	opD3U2))
	, ("primInt32_gt",	(OpGt,	opD3U2))
	, ("primInt32_ge",	(OpGe,	opD3U2))
	, ("primInt32_lt",	(OpLt,	opD3U2))
	, ("primInt32_le",	(OpLe,	opD3U2))

	-- boxed boolean functions
	, ("&&",		(OpAnd,	opD3U2))
	, ("||",		(OpOr,	opD3U2))

	-- boxed float functions
	, ("primFloat32_add",	(OpAdd,	opD3U2))
	, ("primFloat32_sub",	(OpSub,	opD3U2))
	, ("primFloat32_div",	(OpDiv,	opD3U2))
	, ("primFloat32_mul",	(OpMul,	opD3U2))
	, ("primFloat32_eq",	(OpEq,	opD3U2))
	, ("primFloat32_neq",	(OpNeq,	opD3U2))
	, ("primFloat32_gt",	(OpGt,	opD3U2))
	, ("primFloat32_ge",	(OpGe,	opD3U2))
	, ("primFloat32_lt",	(OpLt,	opD3U2))
	, ("primFloat32_le",	(OpLe,	opD3U2)) ]
	

-- primitive functions
primFuns :: Map String Op
primFuns 
 = Map.fromList

	-- unboxed int functions
 	[ ("primInt32U_neg",	OpNeg)
	
	, ("primInt32U_add",	OpAdd)
	, ("primInt32U_sub",	OpSub)
	, ("primInt32U_div",	OpDiv)
	, ("primInt32U_mod",	OpMod)
	, ("primInt32U_mul",	OpMul) 

	-- unboxed int comparisons
	, ("primInt32U_eq",	OpEq)
	, ("primInt32U_neq",	OpNeq)
	, ("primInt32U_gt",	OpGt)
	, ("primInt32U_ge",	OpGe)
	, ("primInt32U_lt",	OpLt)
	, ("primInt32U_le",	OpLe) 
	
	-- unboxed float functions
	, ("primFloat32U_add",	OpEq)
	, ("primFloat32U_sub",	OpSub)
	, ("primFloat32U_div",	OpDiv)
	, ("primFloat32U_mod",	OpMod)
	, ("primFloat32U_mul",	OpMul) 

	-- unboxed int comparisons
	, ("primFloat32U_eq",	OpEq)
	, ("primFloat32U_neq",	OpNeq)
	, ("primFloat32U_gt",	OpGt)
	, ("primFloat32U_ge",	OpGe)
	, ("primFloat32U_lt",	OpLt)
	, ("primFloat32U_le",	OpLe) ]




isUnboxFunctionV :: Var -> Bool
isUnboxFunctionV v
 =  elem (Var.name v)
 	[ "unboxInt32"
	, "unboxFloat32" ]

isBoxFunctionV :: Var -> Bool
isBoxFunctionV v
 = elem	(Var.name v)
 	[ "boxInt32"
	, "boxFloat32" ]
	
