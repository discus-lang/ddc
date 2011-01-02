
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
import DDC.Core.Check
import DDC.Type
import DDC.Var
import DDC.Base.Prim.PrimOp
import DDC.Base.Prim.PrimCast
import DDC.Base.Prim.PrimBoxing
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
	 | Just pt	<- readPrimBoxing (varName v)
	 -> buildApp
		$ (Left $ XPrim (MBox pt) t)
		: psArgs

	-- direct use of unboxing function
	-- note that we must force non-direct objects before unboxing them.
	Left (XVar v t) : psArgs@[Right tR@(TVar kR (UVar vR)), Left x]
	 | kR	== kRegion			
	 , Just pt	<- readPrimUnboxing (varName v)
	 -> if Set.member vR (tableDirectRegions tt) 
		then buildApp
			$ Left (XPrim (MUnbox pt) t)
			: psArgs

		else buildApp
			$ Left (XPrim (MUnbox pt) t) 
			: Right tR 
			: Left (XApp (XPrim MForce (tForceFn (checkedTypeOfExp "Core.Prim.primX1" x))) x)
			: []

	-- primitive arithmetic operators
	Left (XVar v t) : psArgs
	 | Just (op, pt)	<- readPrimOp (varName v)
	 -> buildApp 
		$ Left (XPrim (MOp pt op) t)
		: psArgs
	
	-- primitive casting
	[Left (XVar v t), Left x]
	 | Just (pt1, pt2)	<- readPrimCast (varName v)
	 -> buildApp [Left (XPrim (MCast pt1 pt2) t), Left x]
	
	-- primitive pointer coercion
	[Left (XVar v t), Right t1, Right t2, Left x]
	 | varName v == "coercePtr"
	 -> buildApp [Left (XPrim (MCoercePtr t1 t2) t), Right t1, Right t2, Left x]


	-- not a primitive function.
	_ -> Just xx



