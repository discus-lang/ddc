
module DDC.Core.Lint.Prim
	(checkPrim)
where
import Shared.VarPrim
import DDC.Main.Pretty
import DDC.Core.Exp
import DDC.Core.Lint.Env
import DDC.Type
import DDC.Var
import DDC.Base.DataFormat
import Data.Sequence		(Seq)
import Data.Map			(Map)
import qualified DDC.Var.VarId	as Var
import qualified DDC.Var.PrimId	as Var
import {-# SOURCE #-} DDC.Core.Lint
import qualified Data.Sequence	as Seq


-- | Check an application of a primitive operator.
checkPrim :: Prim -> [Exp] -> Env -> (Type, Seq Effect, Map Var Closure)
checkPrim pp xs env
 = case (pp, xs) of
	(MBox,   [XPrimType r, x])
	 -> let (t, eff, clo)	= checkExp' x env
	    in	( boxedVersionOfUnboxedType r t
		, eff
		, clo)
		
	(MUnbox, [XPrimType r, x])
	 -> let	(t, eff, clo)	= checkExp' x env
	    in	( unboxedVersionOfBoxedType r t
		, eff Seq.|> TApp tRead r
		, clo)


-- | Convert this boxed type to the unboxed version.
boxedVersionOfUnboxedType :: Region -> Type -> Type
boxedVersionOfUnboxedType r tt
	| Just (v, k, _)		<- takeTData tt
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (varId v)
	, Just fmtBoxed			<- dataFormatBoxedOfUnboxed fmt
	= makeTData 
		(primVarFmt NameType 
				(pprStrPlain (varModuleId v) ++ "." ++ baseName) 
				mkBind fmtBoxed)
		(KFun kRegion kValue) 
		[r]


-- | Convert this unboxed type to the boxed version.
unboxedVersionOfBoxedType :: Region -> Type -> Type
unboxedVersionOfBoxedType r1 tt
	| Just (v, k, [r2@(TVar kV _)])	<- takeTData tt
	, kV == kRegion
	, r1 == r2
	, (baseName, mkBind, fmt)	<- splitLiteralVarBind (varId v)
	, Just fmtUnboxed		<- dataFormatUnboxedOfBoxed fmt
	= makeTData
		(primVarFmt NameType 
				(pprStrPlain (varModuleId v) ++ "." ++ baseName)
				mkBind fmtUnboxed)
		kValue
		[]


-- | Split the VarBind for a literal into its components,
--	eg  TInt fmt -> ("Int", TInt, fmt)
splitLiteralVarBind
	:: Var.VarId
	-> ( String
	   , DataFormat -> Var.PrimId
	   , DataFormat)

splitLiteralVarBind (VarIdPrim pid)
 = case pid of
 	Var.TBool  fmt	-> ("Bool",   Var.TBool,   fmt)
	Var.TWord  fmt	-> ("Word",   Var.TWord,   fmt)
	Var.TInt   fmt	-> ("Int",    Var.TInt,    fmt)
	Var.TFloat fmt	-> ("Float",  Var.TFloat,  fmt)
	Var.TChar  fmt	-> ("Char",   Var.TChar,   fmt)
	Var.TString fmt	-> ("String", Var.TString, fmt)



