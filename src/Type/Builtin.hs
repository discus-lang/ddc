
-- | Built-in type constructors.
--	These have special meaning to the compiler.
module Type.Builtin
where
import Type.Exp
import Shared.VarPrim
import Shared.Literal
import DDC.Base.DataFormat
import Shared.Var		(Var)

-- Witness / Witness Kind constructors
--	The contstructor is overloaded to be used for both witnesses and witness kinds
tcConst		= TyConClass TyClassConst	(KForall kRegion (KClass TyClassConst 	 [TIndex 0]))
tcConstT 	= TyConClass TyClassConstT	(KForall kValue  (KClass TyClassConstT	 [TIndex 0]))
tcMutable	= TyConClass TyClassMutable	(KForall kRegion (KClass TyClassMutable	 [TIndex 0]))
tcMutableT	= TyConClass TyClassMutableT	(KForall kValue  (KClass TyClassMutableT [TIndex 0]))
tcLazy		= TyConClass TyClassLazy	(KForall kRegion (KClass TyClassLazy 	 [TIndex 0]))
tcLazyH		= TyConClass TyClassLazyH	(KForall kValue	 (KClass TyClassLazyH	 [TIndex 0]))
tcDirect	= TyConClass TyClassDirect	(KForall kRegion (KClass TyClassDirect	 [TIndex 0]))

tcPurify	= TyConClass TyClassPurify	
			(KForall kRegion 
				(KFun 	(KClass TyClassConst [TIndex 0])
					(KClass TyClassPure  [TEffect primRead [TIndex 0]])))

tcPure		= TyConClass TyClassPure
			(KForall kEffect (KClass TyClassPure [TIndex 0]))

-- Data Type Constructors -------------------------------------------------------------------------
tcBool :: DataFormat -> TyCon
tcBool fmt
 = case fmt of
	Unboxed		-> TyConData (primTBool fmt) kValue
	Boxed		-> TyConData (primTBool fmt) (KFun kRegion kValue)

-- Words
--	have kind (% -> *) for the BoxedBits case 
--	and  kind (*) for the UnboxedBits case.
--
tcWord :: DataFormat -> TyCon
tcWord 	= tcTyDataBits primTWord
tcInt	= tcTyDataBits primTInt
tcFloat	= tcTyDataBits primTFloat
tcChar	= tcTyDataBits primTChar

tcTyDataBits :: (DataFormat -> Var) -> DataFormat -> TyCon
tcTyDataBits mkVar fmt
 = case fmt of 
	Boxed		-> TyConData (mkVar fmt) (KFun kRegion kValue)
	BoxedBits _	-> TyConData (mkVar fmt) (KFun kRegion kValue)
	Unboxed		-> TyConData (mkVar fmt) kValue
	UnboxedBits _	-> TyConData (mkVar fmt) kValue
	
tcString :: DataFormat -> TyCon
tcString fmt
 = case fmt of
	Unboxed		-> TyConData (primTString fmt) (KFun kRegion kValue)
	Boxed		-> TyConData (primTString fmt) (KFun kRegion kValue)


-- | Get the tycon used to represent some literal value
tyConOfLiteralFmt :: LiteralFmt -> TyCon
tyConOfLiteralFmt (LiteralFmt lit fmt)
 = case (lit, fmt) of
 	(LBool _, 	fmt)	-> tcBool fmt
	(LWord _, 	fmt)	-> tcWord fmt
	(LInt _,	fmt)	-> tcInt  fmt
	(LFloat _,	fmt)	-> tcFloat fmt
	(LChar _,	fmt)	-> tcChar fmt
	(LString _,	fmt)	-> tcString fmt


-- | Work out the type associated with a literal value
--	The type returned will have kind (% -> *) if it needs a region variable.
--
typeOfLiteral :: LiteralFmt -> Type
typeOfLiteral litfmt
	= TCon (tyconOfLiteral litfmt)

tyconOfLiteral :: LiteralFmt -> TyCon
tyconOfLiteral (LiteralFmt lit fmt)
 = case lit of
	LBool _		-> tcBool   fmt
	LWord _		-> tcWord   fmt
	LInt _		-> tcInt    fmt
	LFloat _	-> tcFloat  fmt
	LChar _		-> tcChar   fmt
	LString _	-> tcString fmt

