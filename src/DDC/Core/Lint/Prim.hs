{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Lint.Prim
	(checkPrim)
where
import Shared.VarPrim
import DDC.Main.Pretty
import DDC.Main.Error
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

stage	= "DDC.Core.Lint.Prim"

-- | Check an application of a primitive operator.
checkPrim :: Prim -> [Exp] -> Env -> (Type, Seq Effect, Map Var Closure)
checkPrim pp xs env
 = case (pp, xs) of
	(MBox,   [XPrimType r, x])
	 -> let (t, eff, clo)	= checkExp' x env
		Just t'		= boxedVersionOfUnboxedType r t
	    in	( t'
		, eff
		, clo)
		
	(MUnbox, [XPrimType r, x])
	 -> let	(t, eff, clo)	= checkExp' x env
		Just t'		= unboxedVersionOfBoxedType r t
	    in	( t'
		, eff Seq.|> TApp tRead r
		, clo)

	_ -> panic stage $ "checkPrim: not finished for " % (pp, xs)


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


