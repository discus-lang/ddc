
-- | Core.Prim
--	Find direct uses of primitive functions and replace them by XPrim nodes.
--	When we walk down the tree we remember what regions are marked as Direct.
--	Direct regions don't need to be forced before being unboxed.
--
--	We also do some pretend cross-module inlining for critical aritmetic operations
--	where we really want to expose the box\/unbox pairs to the simplifier.
--
--	TODO: Erase forcings on Direct objects in a separate pass.
--
module Core.Prim
	(primGlob)
where
import Core.Util
import Util
import DDC.Core.Exp
import DDC.Core.Glob
import DDC.Type
import DDC.Var
import DDC.Base.Prim.PrimOp
import DDC.Base.Prim.PrimCast
import qualified Data.Set		as Set


-- Table -------------------------------------------------------------------------------------------
data Table
	= Table
	{ tableDirectRegions	:: Set Var }
	
tableZero 
	= Table
	{ tableDirectRegions	= Set.empty }


-- | If this is a witness to constness of a region or type, or purity of an effect
--	then slurp it into the table.
slurpWitnessKind :: Table -> Kind -> Table
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
	XVar{}			-> (tt, xx)
	XPrim{}			-> (tt, xx)

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
--	TODO: 	fix this for over-application. need to split off some of the args
primS :: Table -> Stmt -> (Table, Stmt)
primS tt ss
 = case ss of
	-- enter into XTau
	SBind mV (XTau t x)	
	 -> let Just x2		= primX1 tt x
		(tt3, x3)	= primX tt x2
	    in	(tt3, SBind mV (XTau t x3))
	 
 	SBind mV x		
	 -> let Just x2		= primX1 tt x
		(tt3, x3)	= primX tt x2

	    in  (tt3, SBind mV x3)

-- do the flat rewrite
primX1 :: Table -> Exp -> Maybe Exp
primX1 tt xx
 	| isXApp xx || isXAPP xx	= primX1' tt xx (flattenApps xx)
 	| otherwise			= Just xx
	
primX1' tt xx parts
 = case parts of
 	-- direct use of boxing function
 	Left (XVar v t) : psArgs
	 | isBoxFunctionV v
	 -> buildApp
		$ (Left $ XPrim MBox t)
		: psArgs

	-- direct use of unboxing function
	-- note that we must force non-direct objects before unboxing them.
	Left (XVar v t) : psArgs@[Right tR@(TVar kR (UVar vR)), Left x]
	 | kR	== kRegion			
	 , isUnboxFunctionV v
	 -> if Set.member vR (tableDirectRegions tt) 
		then buildApp
			$ Left (XPrim MUnbox t)
			: psArgs

		else buildApp
			$ Left (XPrim MUnbox t) 
			: Right tR 
			: Left (XApp (XPrim MForce (error "need type of force")) x)
			: []

	-- primitive arithmetic operators
	Left (XVar v t) : psArgs
	 | Just operator	<- readPrimOp (varName v)
	 -> buildApp 
		$ Left (XPrim (MOp operator) t)
		: psArgs
	
	-- primitive casting
	[Left (XVar v t), Right x]
	 | Just (pt1, pt2)	<- readPrimCast (varName v)
	 -> buildApp [Left (XPrim (MCast pt1 pt2) t), Right x]
	
	-- primitive pointer coercion
	[Left (XVar v t), Right t1, Right t2, Left x]
	 | varName v == "coercePtr"
	 -> buildApp [Left (XPrim (MCoercePtr t1 t2) t), Right t1, Right t2, Left x]


	-- not a primitive function.
	_ -> Just xx


-- Detection --------------------------------------------------------------------------------------
-- | Is this the name of a fn that unboxes a value of primitive type.
isBoxFunctionV :: Var -> Bool
isBoxFunctionV v
 = elem	(varName v)
 	[ "boxInt32"
	, "boxFloat32"
	, "boxInt64"
	, "boxFloat64"
 	, "boxWord32"
	, "boxWord64" ]


-- | Is this the name of a fn that unboxes a value of primitive type.
isUnboxFunctionV :: Var -> Bool
isUnboxFunctionV v
 =  elem (varName v)
 	[ "unboxInt32"
	, "unboxFloat32"
	, "unboxInt64"
	, "unboxFloat64"
	, "unboxWord32"
	, "unboxWord64" ]


----------------------------------------------------------------------------------------------------
-- Poor man's cross module inliner.
--   Says what to do with the arguments of a function.
--   We tend to ignore type arguments, and unbox boxed values.

	-- look for binary functions who's arguments can be unboxed
	-- This basically does fake inlining of these functions.
{-	[Left (XVar v t), Right t1, Right t2, Left x1, Left x2]
	 | Just (operator, actions)		<- Map.lookup (varName v) unboxableFuns
	 , tResult				<- checkedTypeOfOpenExp (stage ++ ".primX") xx
	 , Just (vResult, _, tsResult)		<- takeTData tResult
	 , 
	
	 -> buildApp 
		$ Left (XPrim 
		-- apply the actions to the arguments
		psArgs'	= doActions tt actions psArgs
		
		-- generate the primitive 
	   in	XPrim MBox (map XPrimType tsResult ++ [XPrim (MOp operator) psArgs'])



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

   in	x'

doActions _ _ xs = xs



-- Whether to ignore or unbox the argument
data Action
	= Ignore
	| Discard
	| Unbox
	deriving (Show, Eq)

opD3U2	= [Discard, Discard, Discard, Unbox, Unbox]


-- Functions who's arguments can be unboxed.
-- These functions must have these specific names in the prelude.
unboxableFuns :: Map String (PrimOp, [Action])
unboxableFuns
 = Map.fromList
	[ ("&&",		(OpAnd,	opD3U2))
	, ("||",		(OpOr,	opD3U2))

	, ("primInt32_add",	(OpAdd, opD3U2))
	, ("primInt32_sub",	(OpSub, opD3U2))
	, ("primInt32_mul",	(OpMul, opD3U2)) 
	, ("primInt32_eq",	(OpEq,  opD3U2))
	, ("primInt32_neq",	(OpNeq, opD3U2))
	, ("primInt32_lt",	(OpLt,  opD3U2))
	, ("primInt32_gt",	(OpGt,  opD3U2))
	, ("primInt32_le",	(OpLe,  opD3U2)) 
	, ("primInt32_ge",	(OpGe,  opD3U2)) 

	, ("primFloat32_add",	(OpAdd, opD3U2))
	, ("primFloat32_sub",	(OpSub, opD3U2))
	, ("primFloat32_mul",	(OpMul, opD3U2)) 
	, ("primFloat32_eq",	(OpEq,  opD3U2))
	, ("primFloat32_neq",	(OpNeq, opD3U2))
	, ("primFloat32_lt",	(OpLt,  opD3U2))
	, ("primFloat32_gt",	(OpGt,  opD3U2))
	, ("primFloat32_le",	(OpLe,  opD3U2)) 
	, ("primFloat32_ge",	(OpGe,  opD3U2)) ]
-}
