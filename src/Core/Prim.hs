
-- Core.Prim
--	Find direct uses of primitive functions and replace them by XPrim nodes.
--	When we walk down the tree we remember what regions are marked as Direct.
--	Direct regions don't need to be forced before being unboxed.
--
module Core.Prim
	(primGlob)
where
import Core.Util
import Util
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Core.Check.Exp
import DDC.Type
import DDC.Var
import qualified Debug.Trace
import qualified Data.Map		as Map
import qualified Data.Set		as Set

stage	= "Core.Prim"

debug	= False
trace ss x
 = if debug 
 	then Debug.Trace.trace (pprStrPlain ss) x
  	else x


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
 	KApp k (TVar kR (UVar r))
	 | k    == kDirect
	 , kR	== kRegion
	 -> tt { tableDirectRegions 
	 		= Set.insert r (tableDirectRegions tt)}

	_ -> tt

-- Prim --------------------------------------------------------------------------------------------

-- Identify primitive operations
--	Tree should be in snipped form
primGlob :: Glob -> Glob
primGlob glob
 	= mapBindsOfGlob (snd . (primP tableZero)) glob

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
	 -> let	ks	=  map (kindOfType . snd) vts
	 	tt2	= foldl' slurpWitnessKind tt ks
		
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

	XTau t x
	 -> let (tt', x')	= primX tt x
	    in	(tt', XTau t x')

	XApp x1 x2
	 -> let (tt2, x1')	= primX tt  x1
	        (tt3, x2')	= primX tt2 x2
	    in	(tt3, XApp x1' x2')

	XPrim p xs
	 -> let (tt', xs')	= mapAccumL primX tt xs
	    in	(tt', XPrim p xs')

	XPrimType{}		-> (tt, xx)

	XLit{}			-> (tt, xx)


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
	, XVar v t : psArgs@[XPrimType tR@(TVar kR (UVar vR)), x] 	<- parts
	, kR	== kRegion			
	, isUnboxFunctionV v
	= if Set.member vR (tableDirectRegions tt) 
		then XPrim MUnbox psArgs

		-- have to force non-direct objects before unboxing them
		else XPrim MUnbox [XPrimType tR, XPrim MForce [x]]


	-- look for functions in the prim table
 	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, Just operator				<- Map.lookup (varName v) primFuns
	= XPrim (MOp operator) psArgs
	
	-- look for functions who's arguments can be unboxed
 	| Just parts				<- flattenAppsEff xx
	, (XVar v t : psArgs)			<- parts
	, Just (operator, actions)		<- Map.lookup (varName v) unboxableFuns
	, tResult				<- checkedTypeOfOpenExp (stage ++ ".primX") xx
	, Just (vResult, _, tsResult)		<- takeTData tResult
	, length psArgs == length actions
	= trace ( "primX:\n"
		% "    xx      = " % xx 	% "\n"
		% "    parts   = " % parts	% "\n"
		% "    tResult = " % tResult	% "\n") 
	   $ let	
		-- apply the actions to the arguments
		psArgs'	= doActions tt actions psArgs
		
		-- generate the primitive 
	   in	XPrim MBox (map XPrimType tsResult ++ [XPrim (MOp operator) psArgs'])
	
	| otherwise		
	= xx

-- | Perform these actions on this list of expressions
doActions :: Table -> [Action] -> [Exp] -> [Exp]
doActions tt (Discard:as) (x:xs)
	= doActions tt as xs

doActions tt (Ignore:as) (x:xs)
	= x : doActions tt as xs
	
doActions tt (Unbox:as) (x:xs)

 = let	tX	= checkedTypeOfOpenExp (stage ++ ".doActions") x

 	Just (_, _, [tR@(TVar _ (UVar vR))])
		= takeTData tX
 		

	x'	| Set.member vR (tableDirectRegions tt) 
		= XPrim MUnbox [XPrimType tR, x] 
			: doActions tt as xs
		
		| otherwise
		= XPrim MUnbox [XPrimType tR, XPrim MForce [x]] 
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
 	XApp{}	-> Just $ splitAppsUsingPrimType xx
	XAPP{}	-> Just $ splitAppsUsingPrimType xx
	_	-> Nothing


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
-- opD1U1	= [Discard, Discard, Unbox]
opD3U2	= [Discard, Discard, Discard, Unbox, Unbox]

----------------------------------------------------------------------------------------------------
-- TODO: the code for these tables could be a lot cleverer
--	explicitly listing each member won't be the right way when we want
--	to support all the other primitive types as well.


-- functions who's arguments can be unboxed
unboxableFuns :: Map String (PrimOp, [Action])
unboxableFuns
 = Map.fromList

	[
	-- boxed boolean functions
	  ("&&",		(OpAnd,	opD3U2))
	, ("||",		(OpOr,	opD3U2))

	-- boxed float64 functions
	, ("primFloat64_add",	(OpAdd,	opD3U2))
	, ("primFloat64_sub",	(OpSub,	opD3U2))
	, ("primFloat64_div",	(OpDiv,	opD3U2))
	, ("primFloat64_mul",	(OpMul,	opD3U2))
	, ("primFloat64_eq",	(OpEq,	opD3U2))
	, ("primFloat64_neq",	(OpNeq,	opD3U2))
	, ("primFloat64_gt",	(OpGt,	opD3U2))
	, ("primFloat64_ge",	(OpGe,	opD3U2))
	, ("primFloat64_lt",	(OpLt,	opD3U2))
	, ("primFloat64_le",	(OpLe,	opD3U2)) ]
	

-- primitive functions
primFuns :: Map String PrimOp
primFuns 
 = Map.fromList

	-- unboxed ptr functions
	[ ("primPtrVoid_eq",	OpEq)

	-- Word32U
	, ("primWord32U_neg",	OpNeg) 

	, ("primWord32U_add",	OpAdd)
	, ("primWord32U_sub",	OpSub)
	, ("primWord32U_mul",	OpMul)
	, ("primWord32U_div",	OpDiv)
	, ("primWord32U_mod",	OpMod)

	, ("primWord32U_eq",	OpEq)
	, ("primWord32U_neq",	OpNeq)
	, ("primWord32U_gt",	OpGt)
	, ("primWord32U_ge",	OpGe)
	, ("primWord32U_lt",	OpLt)
	, ("primWord32U_le",	OpLe) 

	-- Int32U
 	, ("primInt32U_neg",	OpNeg)
	
	, ("primInt32U_add",	OpAdd)
	, ("primInt32U_sub",	OpSub)
	, ("primInt32U_mul",	OpMul) 
	, ("primInt32U_div",	OpDiv)
	, ("primInt32U_mod",	OpMod)

	, ("primInt32U_eq",	OpEq)
	, ("primInt32U_neq",	OpNeq)
	, ("primInt32U_gt",	OpGt)
	, ("primInt32U_ge",	OpGe)
	, ("primInt32U_lt",	OpLt)
	, ("primInt32U_le",	OpLe) 

	-- Int64U
 	, ("primInt64U_neg",	OpNeg)
	
	, ("primInt64U_add",	OpAdd)
	, ("primInt64U_sub",	OpSub)
	, ("primInt64U_mul",	OpMul) 
	, ("primInt64U_div",	OpDiv)
	, ("primInt64U_mod",	OpMod)

	, ("primInt64U_eq",	OpEq)
	, ("primInt64U_neq",	OpNeq)
	, ("primInt64U_gt",	OpGt)
	, ("primInt64U_ge",	OpGe)
	, ("primInt64U_lt",	OpLt)
	, ("primInt64U_le",	OpLe) 

	
	-- Float32U
	, ("primFloat32U_neg",	OpNeg)

	, ("primFloat32U_add",	OpAdd)
	, ("primFloat32U_sub",	OpSub)
	, ("primFloat32U_mul",	OpMul) 
	, ("primFloat32U_div",	OpDiv)

	, ("primFloat32U_eq",	OpEq)
	, ("primFloat32U_neq",	OpNeq)
	, ("primFloat32U_gt",	OpGt)
	, ("primFloat32U_ge",	OpGe)
	, ("primFloat32U_lt",	OpLt)
	, ("primFloat32U_le",	OpLe) ]




isUnboxFunctionV :: Var -> Bool
isUnboxFunctionV v
 =  elem (varName v)
 	[ "unboxInt32"
	, "unboxFloat32"
	, "unboxInt64"
	, "unboxFloat64" ]

isBoxFunctionV :: Var -> Bool
isBoxFunctionV v
 = elem	(varName v)
 	[ "boxInt32"
	, "boxFloat32"
	, "boxInt64"
	, "boxFloat64" ]
	
