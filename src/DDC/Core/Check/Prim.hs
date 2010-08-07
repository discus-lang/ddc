{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Check.Prim
	( checkPrim
	, unboxedVersionOfBoxedType
	, boxedVersionOfUnboxedType)
where
import Shared.VarPrim
import Core.Util.Bits
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Core.Exp
import DDC.Core.Check.Env
import DDC.Type
import DDC.Var
import DDC.Base.DataFormat
import DDC.Type.EffectStore		(EffectStore)
import DDC.Type.ClosureStore		(ClosureStore)
import qualified DDC.Type.EffectStore	as Eff
import qualified DDC.Type.ClosureStore	as Clo
import qualified DDC.Var.VarId		as Var
import qualified DDC.Var.PrimId		as Var
import {-# SOURCE #-} DDC.Core.Check.Exp

stage	= "DDC.Core.Check.Prim"

-- | Check an application of a primitive operator.
checkPrim 
	:: Int -> Prim -> [Exp] -> Env 
	-> ([Exp], Type, EffectStore, ClosureStore)

checkPrim n pp xs env
 = case (pp, xs) of
	(MBox,   [XPrimType r, x])
	 -> let (x', t, eff, clo)	= checkExp' (n+1) x env
		Just t'			= boxedVersionOfUnboxedType r t
	    in	( [XPrimType r, x']
		, t'
		, eff
		, clo)
		
	(MUnbox, [XPrimType r, x])
	 -> let	(x', t, eff, clo)	= checkExp' (n+1) x env
		Just t'			= unboxedVersionOfBoxedType r t
	    in	( [XPrimType r, x']
		, t'
		, Eff.union eff (Eff.fromEffect $ TApp tRead r)
		, clo)

	(MForce, [x])
	 -> let (x', t, eff, clo)	= checkExp' (n+1) x env
	    in	( [x']
		, t
		, eff
		, clo)

	(MOp op, _)
	 -> checkPrimOpApp (n+1) op xs env 

	-- TODO: This was never a good idea. Just use regular application.
	(MCall _, _)
	 | Just x'		<- buildAppUsingPrimType xs
	 , (_, t, eff, clo)	<- checkExp' (n+1) x' env
	 -> (xs, t, eff, clo)

	_ -> panic stage $ vcat
		[ "checkPrim: no match for " % (pp, xs)
		, "During: " % envCaller env ]


-- | Reconstruct the type and effect of an operator application.
--	Primops don't have real type sigs, so we have to do this manually.
--	It'd probably be bett
checkPrimOpApp 
	:: Int
	-> PrimOp -> [Exp] -> Env
	-> ([Exp], Type, EffectStore, ClosureStore)

checkPrimOpApp n op xs env
 = let	(xs' :: [Exp], ts :: [Type], effs :: [EffectStore], clos :: [ClosureStore])
 		= unzip4
		$ map (\x -> checkExp' n x env) xs

	eff	= Eff.unions effs
	clo	= Clo.unions clos

	result
		-- arithmetic ops
		| op == OpNeg
		, [t1]		<- ts
		, isUnboxedNumericType t1
		= (xs', t1, eff, clo)

		| elem op [OpAdd, OpSub, OpMul, OpDiv, OpMod]
		, [t1, t2]	<- ts
		, isUnboxedNumericType t1
		, t1 == t2
		= (xs', t1, eff, clo)

		-- comparison operators
		| elem op [OpEq, OpNeq, OpGt, OpGe, OpLt, OpLe]
		, [t1, t2]	<- ts
		, isUnboxedNumericType t1
		, t1 == t2
		= (xs', makeTData (primTBool Unboxed) kValue [], eff, clo)

		-- boolean operators
		| elem op [OpAnd, OpOr]
		, [t1, t2]		<- ts
		, Just (v, _, [])	<- takeTData t1
		, v == (primTBool Unboxed)
		, t1 == t2
		= (xs', makeTData (primTBool Unboxed) kValue [], eff, clo)

		| otherwise
		= panic stage
		$ "reconOpApp: no match for " % op % " " % xs % "\n"

   in result

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = unzip4' [] [] [] []

unzip4' ia ja ka la []
	= (reverse ia, reverse ja, reverse ka, reverse la)

unzip4' ia ja ka la ((i, j, k, l) : rest)
	= unzip4' (i : ia) (j : ja) (k : ka) (l : la) rest
	

-- | Convert this boxed type to the unboxed version.
boxedVersionOfUnboxedType :: Region -> Type -> Maybe Type
boxedVersionOfUnboxedType r tt
	| Just (v, _, _)		<- takeTData tt
	, Just (baseName, mkBind, fmt)	<- splitLiteralVarBind (varId v)
	, Just fmtBoxed			<- dataFormatBoxedOfUnboxed fmt
	= Just $ makeTData 
		(primVarFmt NameType 
				(pprStrPlain (varModuleId v) ++ "." ++ baseName) 
				mkBind fmtBoxed)
		(KFun kRegion kValue) 
		[r]
		
	| otherwise
	= Nothing


-- | Convert this unboxed type to the boxed version.
unboxedVersionOfBoxedType :: Region -> Type -> Maybe Type
unboxedVersionOfBoxedType r1 tt
	| Just (v, _, [r2@(TVar kV _)])	<- takeTData tt
	, kV == kRegion
	, r1 == r2
	, Just (baseName, mkBind, fmt)	<- splitLiteralVarBind (varId v)
	, Just fmtUnboxed		<- dataFormatUnboxedOfBoxed fmt
	= Just $ makeTData
		(primVarFmt NameType 
				(pprStrPlain (varModuleId v) ++ "." ++ baseName)
				mkBind fmtUnboxed)
		kValue
		[]

	| otherwise
	= Nothing


-- | Check whether a type is an unboxed numeric type
isUnboxedNumericType :: Type -> Bool
isUnboxedNumericType tt
 	| Just (v, _, []) <- takeTData tt
	, isUnboxedNumericType_varId (varId v)
	= True

	-- treat pointers as numeric types
	| Just (v, _, _) <- takeTData tt
	, v == primTPtrU
	= True
	
	| otherwise
	= False


isUnboxedNumericType_varId vid
 = case vid of
 	Var.VarIdPrim (Var.TWord _)		-> True
	Var.VarIdPrim (Var.TInt _)		-> True
	Var.VarIdPrim (Var.TFloat _)		-> True
	_					-> False


-- | Split the VarBind for a literal into its components,
--	eg  TInt fmt -> ("Int", TInt, fmt)
splitLiteralVarBind
	:: Var.VarId
	-> Maybe
	   ( String
	   , DataFormat -> Var.PrimId
	   , DataFormat)

splitLiteralVarBind (VarIdPrim pid)
 = case pid of
 	Var.TBool  fmt	-> Just ("Bool",   Var.TBool,   fmt)
	Var.TWord  fmt	-> Just ("Word",   Var.TWord,   fmt)
	Var.TInt   fmt	-> Just ("Int",    Var.TInt,    fmt)
	Var.TFloat fmt	-> Just ("Float",  Var.TFloat,  fmt)
	Var.TChar  fmt	-> Just ("Char",   Var.TChar,   fmt)
	Var.TString fmt	-> Just ("String", Var.TString, fmt)
	_		-> Nothing

splitLiteralVarBind _
 	= Nothing


