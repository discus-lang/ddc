{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Type constructors.
module DDC.Type.Exp.TyCon
	( TyCon		 (..)
	, TyConEffect    (..)
	, TyConClosure   (..)
	, TyConWitness 	 (..)
	, TyConElaborate (..)
	, takeTyConWitnessOfKiCon
	, takeKiConOfTyConWitness)
where
import DDC.Var
import DDC.Type.Exp.KiCon
import {-# SOURCE #-} DDC.Type.Exp
import {-# SOURCE #-} DDC.Type.Data.Base

-- | Type constructors.
data TyCon
	-- | Function type constructor.
	= TyConFun

	-- | A data type constructor.
	| TyConData
		{ tyConName		:: !Var
		, tyConDataKind		:: !Kind 
		, tyConDataDef		:: !(Maybe DataDef) }

	-- | An effect type constructor.
	| TyConEffect
		{ tyConEffect		:: !TyConEffect
		, tyConEffectKind	:: !Kind }
		
	-- | A closure type constructor.
	| TyConClosure
		{ tyConClosure		:: !TyConClosure
		, tyConClosureKind	:: !Kind }

	-- | A witness type constructor.
	| TyConWitness
		{ tyConWitness		:: !TyConWitness
		, tyConWitnessKind 	:: !Kind }

	-- | An elaboration of some other type. 
	--   These are desugared in Type.Util.Elaborate
	| TyConElaborate
		{ tyConElaborate	:: !TyConElaborate
		, tyConElaborateKind	:: !Kind }

	deriving (Show)

-- When comparing `TyCons` we usually don't want to worry about any
-- attached kinds or `DataDefs`.
instance Eq TyCon where
 (==) tc1 tc2
  = case (tc1, tc2) of
	(TyConFun,  		TyConFun)		-> True
	(TyConData	v1 _ _,	TyConData	v2 _ _)	-> v1 == v2
	(TyConEffect	v1 _,	TyConEffect 	v2 _)	-> v1 == v2
	(TyConClosure	v1 _,	TyConClosure	v2 _)	-> v1 == v2
	(TyConWitness	v1 _,	TyConWitness	v2 _)	-> v1 == v2
	(TyConElaborate	v1 _,	TyConElaborate	v2 _)	-> v1 == v2
	_						-> False


-- TyConEffect ------------------------------------------------------------------------------------
-- | Effect type constructors.
data TyConEffect
	-- | Some user defined top-level effect.
	= TyConEffectTop Var
	
	-- Baked in effect constructors.
	| TyConEffectRead
	| TyConEffectHeadRead
	| TyConEffectDeepRead
	| TyConEffectWrite
	| TyConEffectDeepWrite
	deriving (Show, Eq)


-- TyConClosure -----------------------------------------------------------------------------------
-- | Closure type constructors.
--   These aren't defined by the user, they're all builtin.
data TyConClosure

	-- | Lift a value type or region to a closure term. 
	= TyConClosureFreeType   Var

	-- | Lift a region to a closure term. 
	--   This also contains a tag variable which must be in the value namespace.
	| TyConClosureFreeRegion Var

	-- | Tag a closure with a variable.
	| TyConClosureFree Var
	
	| TyConClosureDanger
	deriving (Show, Eq)
	

-- TyConWitness -----------------------------------------------------------------------------------
-- | Witness type constructors.
data TyConWitness
	-- | Make a witness of some user-defined data type class.
	--   This is used in an intermediate stage in the core language, before we've
	--   introduced dictionary parameters.
	--   The var is the name of the class, like Eq, and the constructor is pretty
	--   printed like MkEq.
	= TyConWitnessMkVar Var		
		
	-- Builtin witness constructors.
	| TyConWitnessMkConst		
	| TyConWitnessMkDeepConst
	| TyConWitnessMkMutable	
	| TyConWitnessMkDeepMutable
	| TyConWitnessMkLazy		
	| TyConWitnessMkHeadLazy
	| TyConWitnessMkDirect
	| TyConWitnessMkPurify
	| TyConWitnessMkPure
	| TyConWitnessMkEmpty
	deriving (Show, Eq)


-- | For a given witness kind constructor, 
--	take the associated witness type constructor.
takeTyConWitnessOfKiCon :: KiCon -> Maybe TyConWitness
takeTyConWitnessOfKiCon kiCon
 = case kiCon of
	KiConVar v			-> Just $ TyConWitnessMkVar v
	KiConConst 			-> Just TyConWitnessMkConst
	KiConDeepConst			-> Just TyConWitnessMkDeepConst
	KiConMutable			-> Just TyConWitnessMkMutable
	KiConDeepMutable		-> Just TyConWitnessMkDeepMutable
	KiConLazy			-> Just TyConWitnessMkLazy
	KiConHeadLazy			-> Just TyConWitnessMkHeadLazy
	KiConDirect			-> Just TyConWitnessMkDirect
	KiConPure			-> Just TyConWitnessMkPure
	KiConEmpty			-> Just TyConWitnessMkEmpty
	_				-> Nothing


-- | For a given witness type constructor, 
--	take the associated witness kind constructor.
takeKiConOfTyConWitness :: TyConWitness -> Maybe KiCon
takeKiConOfTyConWitness tyCon
 = case tyCon of
	TyConWitnessMkVar v		-> Just $ KiConVar v
	TyConWitnessMkConst		-> Just $ KiConConst
	TyConWitnessMkDeepConst		-> Just $ KiConDeepConst
	TyConWitnessMkMutable		-> Just $ KiConMutable
	TyConWitnessMkDeepMutable	-> Just $ KiConDeepMutable
	TyConWitnessMkLazy		-> Just $ KiConLazy
	TyConWitnessMkHeadLazy		-> Just $ KiConHeadLazy
	TyConWitnessMkDirect		-> Just $ KiConDirect
	TyConWitnessMkPure		-> Just $ KiConPure
	TyConWitnessMkEmpty		-> Just $ KiConEmpty
	_				-> Nothing


-- TyConElaborate ---------------------------------------------------------------------------------	
-- | Helps with defining foreign function interfaces.
--	Used in source types only. 
--	Desuared before type inference.
data TyConElaborate
	= TyConElaborateRead		-- ^ Read from a type.
	| TyConElaborateWrite		-- ^ Write to a type.
	| TyConElaborateModify		-- ^ Read and write to a type.
	deriving (Show, Eq)
