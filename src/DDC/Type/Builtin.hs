{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Short names for built-in types and kinds.
module DDC.Type.Builtin 
	( 
	-- * Atomic kind constructors
	  kBox, kValue, kRegion, kEffect, kClosure
	
	-- * Witness kind constructors
	, kConst,   kDeepConst
	, kMutable, kDeepMutable
	, kLazy,    kHeadLazy
	, kDirect
	, kPure
	, kEmpty
	
	-- * Bottom
	, tBot

	-- * Data types
	, tPtrU, tBool, tWord, tInt, tFloat

	-- * Effects
	, tPure
	, tRead,  tDeepRead, tHeadRead
	, tWrite, tDeepWrite
	
	-- * Closure types
	, tEmpty
	, tFree, tFreeType, tFreeRegion, tDanger
	
	-- * Witness types
	, tMkConst,   tMkDeepConst
	, tMkMutable, tMkDeepMutable
	, tMkLazy,    tMkHeadLazy
	, tMkDirect
	, tMkPurify,  tMkPure
	
	-- * Elaboration constructors
	, tElaborateRead
	, tElaborateWrite
	, tElaborateModify
	
	-- * Type constructors
	, tcPtrU
	, tcBool
	, tcWord
	, tcInt
	, tcFloat
	, tcChar
	, tcTyDataBits
	, tcString

	-- * Getting types of literals
	, tyConOfLiteralFmt)
where
import Shared.VarPrim
import DDC.Base.Literal
import DDC.Base.DataFormat
import DDC.Type.Exp
import DDC.Var


-- Kinds  -----------------------------------------------------------------------------------------

-- Atomic kinds -------------------------------------------
kBox		= KCon KiConBox		SBox
kValue		= KCon KiConValue	SBox
kRegion		= KCon KiConRegion	SBox
kEffect		= KCon KiConEffect	SBox
kClosure	= KCon KiConClosure	SBox

-- Witness kinds ------------------------------------------
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


-- Types ------------------------------------------------------------------------------------------
tBot k		= TSum k	[]

-- Value types --------------------------------------------
tPtrU		= TCon $ tcPtrU

tBool   fmt	= TCon $ tcBool   fmt
tInt    fmt	= TCon $ tcInt    fmt
tWord   fmt	= TCon $ tcWord   fmt
tFloat  fmt	= TCon $ tcFloat  fmt


-- Effect types -------------------------------------------
tPure		= TSum kEffect  []

tRead		= TCon $ TyConEffect TyConEffectRead
		$ KFun kRegion kEffect

tDeepRead	= TCon $ TyConEffect TyConEffectDeepRead
		$ KFun kValue kEffect

tHeadRead	= TCon $ TyConEffect TyConEffectHeadRead
		$ KFun kValue kEffect

tWrite		= TCon $ TyConEffect TyConEffectWrite
		$ KFun kRegion kEffect

tDeepWrite	= TCon $ TyConEffect TyConEffectDeepWrite
		$ KFun kValue kEffect


-- Closure types ------------------------------------------
tEmpty		= TSum kClosure []

tFree v		= TCon $ TyConClosure (TyConClosureFree v) 
		$ KFun kClosure kClosure

tFreeType v	= TCon $ TyConClosure (TyConClosureFreeType v) 
		$ KFun kValue kClosure

tFreeRegion v	= TCon $ TyConClosure (TyConClosureFreeRegion v) 
		$ KFun kRegion kClosure
		
tDanger 	= TCon $ TyConClosure TyConClosureDanger
		$ KFun kRegion (KFun kBox kClosure)


-- Witness types ------------------------------------------
tMkConst	= TCon $ TyConWitness TyConWitnessMkConst	
		$ KFun kRegion (KApp kConst		(TVar kRegion $ UIndex 0))

tMkDeepConst 	= TCon $ TyConWitness TyConWitnessMkDeepConst
		$ KFun kValue  (KApp kDeepConst		(TVar kRegion $ UIndex 0))

tMkMutable	= TCon $ TyConWitness TyConWitnessMkMutable
	 	$ KFun kRegion (KApp kMutable		(TVar kRegion $ UIndex 0))

tMkDeepMutable	= TCon $ TyConWitness TyConWitnessMkDeepMutable
 		$ KFun kValue  (KApp kDeepMutable	(TVar kRegion $ UIndex 0))

tMkLazy		= TCon $ TyConWitness TyConWitnessMkLazy
		$ KFun kRegion (KApp kLazy		(TVar kRegion $ UIndex 0))

tMkHeadLazy	= TCon $ TyConWitness TyConWitnessMkHeadLazy
		$ KFun kValue  (KApp kHeadLazy		(TVar kRegion $ UIndex 0))

tMkDirect	= TCon $ TyConWitness TyConWitnessMkDirect
		$ KFun kRegion (KApp kDirect		(TVar kRegion $ UIndex 0))

tMkPurify	= TCon $ TyConWitness TyConWitnessMkPurify	
		$ KFun kRegion 
			(KFun 	(KApp kConst (TVar kRegion $ UIndex 0))
				(KApp kPure  (TApp tRead (TVar kRegion $ UIndex 1))))

tMkPure		= TCon $ TyConWitness TyConWitnessMkPure
		$ KFun kEffect (KApp kPure (TVar kEffect $ UIndex 0))
		
-- Elaboration constructors -------------------------------
tElaborateRead  = TCon $ TyConElaborate TyConElaborateRead
		$ KFun kValue kValue

tElaborateWrite	= TCon $ TyConElaborate TyConElaborateWrite
		$ KFun kValue kValue

tElaborateModify = TCon $ TyConElaborate TyConElaborateModify
		$ KFun kValue kValue



-- Type Constructors  -----------------------------------------------------------------------------

tcPtrU :: TyCon
tcPtrU	= TyConData primTPtrU (kValue `KFun` kValue) Nothing

-- Numeric types ------------------------------------------
-- | Get the type constructor for a bool of this format.
tcBool :: DataFormat -> TyCon
tcBool	= tcTyDataBits primTBool
	
-- | Get the type constructor of a word of this format.
tcWord  :: DataFormat -> TyCon
tcWord 	= tcTyDataBits primTWord

-- | Get the type constructor of an int of this format.
tcInt   :: DataFormat -> TyCon
tcInt	= tcTyDataBits primTInt

-- | Get the type constructor of a float of this format.
tcFloat :: DataFormat -> TyCon
tcFloat	= tcTyDataBits primTFloat


-- | Make the type constructor of something of this format.
tcTyDataBits :: (DataFormat -> Var) -> DataFormat -> TyCon
tcTyDataBits mkVar fmt
 = case fmt of 
	Boxed		-> TyConData (mkVar fmt) (KFun kRegion kValue) Nothing
	BoxedBits _	-> TyConData (mkVar fmt) (KFun kRegion kValue) Nothing
	Unboxed		-> TyConData (mkVar fmt) kValue Nothing
	UnboxedBits _	-> TyConData (mkVar fmt) kValue Nothing


-- Characters and Strings ---------------------------------
-- | Get the type constructor of a char of this format.
--   This gives either Char32 or Word32#, for Boxed and Unboxed character respectively.
--   Other formats yield `Nothing`.
tcChar  :: DataFormat -> Maybe TyCon
tcChar fmt
 = case fmt of
	Unboxed		-> Just $ TyConData (primTWord (UnboxedBits 32)) kValue Nothing
	Boxed		-> Just $ TyConData (primTChar (BoxedBits   32)) (KFun kRegion kValue) Nothing
	_		-> Nothing
	

-- | Get the type constructor of a string of this format.
--   This gives either String or String#. Other formats yield `Nothing`.
--   Note that we actually pass unboxed strings around as (Ptr# (String# %r1)).
tcString :: DataFormat -> Maybe TyCon
tcString fmt
 = case fmt of
	Unboxed		-> Just $ TyConData (primTString fmt) (KFun kRegion kValue) Nothing
	Boxed		-> Just $ TyConData (primTString fmt) (KFun kRegion kValue) Nothing
	_		-> Nothing

	
-- Utils --------------------------------------------------
-- | Get the type constructor used to represent some literal value.
--   This gives `Nothing` for invalid literal formats like (Boxed 32) LStrings.
tyConOfLiteralFmt :: LiteralFmt -> Maybe TyCon
tyConOfLiteralFmt (LiteralFmt lit fmt)
 = case (lit, fmt) of
 	(LBool _, 	fmt')	-> Just $ tcBool   fmt'
	(LWord _, 	fmt')	-> Just $ tcWord   fmt'
	(LInt _,	fmt')	-> Just $ tcInt    fmt'
	(LFloat _,	fmt')	-> Just $ tcFloat  fmt'
	(LChar _,	fmt')	-> tcChar   fmt'
	(LString _,	fmt')	-> tcString fmt'


