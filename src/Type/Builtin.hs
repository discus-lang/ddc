
-- | Short names for built-in types and kinds.
module Type.Builtin 
where
import Type.Exp
import Shared.VarPrim
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Var

-- Kind Constructors ------------------------------------------------------------------------------
-- Atomic kind constructors.
kValue		= KCon KiConValue	SBox
kRegion		= KCon KiConRegion	SBox
kEffect		= KCon KiConEffect	SBox
kClosure	= KCon KiConClosure	SBox

-- Witness kind constructors.
kConst		= KCon KiConConst
		$ SFun kRegion  SProp

kDeepConst	= KCon KiConDeepConst
		$ SFun kValue   SProp

kMutable	= KCon KiConMutable
		$ SFun kRegion  SProp

kDeepMutable	= KCon KiConDeepMutable	
		$ SFun kValue   SProp

kLazy		= KCon KiConLazy
		$ SFun kRegion  SProp

kHeadLazy	= KCon KiConHeadLazy
		$ SFun kValue   SProp

kDirect		= KCon KiConDirect
		$ SFun kRegion  SProp

kPure		= KCon KiConPure
		$ SFun kEffect  SProp

kEmpty		= KCon KiConEmpty
		$ SFun kClosure SProp


-- Witness Type Constructors ----------------------------------------------------------------------
tMkConst	= TCon $ TyConWitness TyConWitnessMkConst	
		$ KFun kRegion (KApps kConst		[TIndex 0])

tMkDeepConst 	= TCon $ TyConWitness TyConWitnessMkDeepConst
		$ KFun kValue  (KApps kDeepConst	[TIndex 0])

tMkMutable	= TCon $ TyConWitness TyConWitnessMkMutable
	 	$ KFun kRegion (KApps kMutable		[TIndex 0])

tMkDeepMutable	= TCon $ TyConWitness TyConWitnessMkDeepMutable
 		$ KFun kValue  (KApps kDeepMutable	[TIndex 0])

tMkLazy		= TCon $ TyConWitness TyConWitnessMkLazy
		$ KFun kRegion (KApps kLazy		[TIndex 0])

tMkHeadLazy	= TCon $ TyConWitness TyConWitnessMkHeadLazy
		$ KFun kValue  (KApps kHeadLazy		[TIndex 0])

tMkDirect	= TCon $ TyConWitness TyConWitnessMkDirect
		$ KFun kRegion (KApps kDirect		[TIndex 0])

tMkPurify	= TCon $ TyConWitness TyConWitnessMkPurify	
		$ KFun kRegion 
			(KFun 	(KApps kConst [TIndex 1])
				(KApps kPure  [TEffect primRead [TIndex 1]]))

tMkPure		= TCon $ TyConWitness TyConWitnessMkPure
		$ KFun kEffect (KApps kPure [TIndex 0])


-- Type Constructors -------------------------------------------------------------------------
tPure	= TBot kEffect
tEmpty	= TBot kClosure


-- | Get the type constructor for a bool of this format.
tcBool :: DataFormat -> TyCon
tcBool fmt
 = case fmt of
	Unboxed		-> TyConData (primTBool fmt) kValue
	Boxed		-> TyConData (primTBool fmt) (KFun kRegion kValue)


-- | Get the type constructor of a word of this format.
tcWord  :: DataFormat -> TyCon
tcWord 	= tcTyDataBits primTWord


-- | Get the type constructor of an int of this format.
tcInt   :: DataFormat -> TyCon
tcInt	= tcTyDataBits primTInt


-- | Get the type constructor of a float of this format.
tcFloat :: DataFormat -> TyCon
tcFloat	= tcTyDataBits primTFloat


-- | Get the type constructor of a char of this format.
tcChar  :: DataFormat -> TyCon
tcChar	= tcTyDataBits primTChar


-- | Make the type constructor of something of this format.
tcTyDataBits :: (DataFormat -> Var) -> DataFormat -> TyCon
tcTyDataBits mkVar fmt
 = case fmt of 
	Boxed		-> TyConData (mkVar fmt) (KFun kRegion kValue)
	BoxedBits _	-> TyConData (mkVar fmt) (KFun kRegion kValue)
	Unboxed		-> TyConData (mkVar fmt) kValue
	UnboxedBits _	-> TyConData (mkVar fmt) kValue
	

-- | Get the type constructor of a string of this format.
tcString :: DataFormat -> TyCon
tcString fmt
 = case fmt of
	Unboxed		-> TyConData (primTString fmt) (KFun kRegion kValue)
	Boxed		-> TyConData (primTString fmt) (KFun kRegion kValue)


-- | Get the type constructor used to represent some literal value
tyConOfLiteralFmt :: LiteralFmt -> TyCon
tyConOfLiteralFmt (LiteralFmt lit fmt)
 = case (lit, fmt) of
 	(LBool _, 	fmt)	-> tcBool fmt
	(LWord _, 	fmt)	-> tcWord fmt
	(LInt _,	fmt)	-> tcInt  fmt
	(LFloat _,	fmt)	-> tcFloat fmt
	(LChar _,	fmt)	-> tcChar fmt
	(LString _,	fmt)	-> tcString fmt


-- | Get the type associated with a literal value.
typeOfLiteral :: LiteralFmt -> Type
typeOfLiteral litfmt
	= TCon (tyconOfLiteral litfmt)


-- | Get the type constructor associated with a literal value.
tyconOfLiteral :: LiteralFmt -> TyCon
tyconOfLiteral (LiteralFmt lit fmt)
 = case lit of
	LBool _		-> tcBool   fmt
	LWord _		-> tcWord   fmt
	LInt _		-> tcInt    fmt
	LFloat _	-> tcFloat  fmt
	LChar _		-> tcChar   fmt
	LString _	-> tcString fmt


