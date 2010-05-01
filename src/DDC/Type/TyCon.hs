
-- | Type constructors.
module DDC.Type.TyCon
	( TyCon		(..)
	, TyConWitness	(..)
	, takeTyConWitnessOfKiCon
	, takeKiConOfTyConWitness)
where
import DDC.Var
import DDC.Type.KiCon
import {-# SOURCE #-} Type.Exp


-- | Type constructors
data TyCon
	-- Function type constructor.
	= TyConFun

	-- A data type constructor.
	| TyConData
		{ tyConName		:: !Var
		, tyConDataKind		:: !Kind }

	-- A witness type constructor.
	| TyConWitness
		{ tyConWitness		:: !TyConWitness
		, tyConWitnessKind 	:: !Kind }

	deriving (Show, Eq)


-- | Witness type constructors.
data TyConWitness
	= 
	-- | Make a witness of some user-defined data type class.
	--   This is used in an intermediate stage in the core language, before we've
	--   introduced dictionary parameters.
	--   The var is the name of the class, like Eq, and the constructor is pretty
	--   printed like MkEq.
	  TyConWitnessMkVar Var		
		
	-- Baked in witness constructors.
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
	
