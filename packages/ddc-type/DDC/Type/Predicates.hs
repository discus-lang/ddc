
-- | Predicates on type expressions.
module DDC.Type.Predicates
        ( isBot
        , isAtomT
        , isDataKind
        , isRegionKind
        , isEffectKind
        , isClosureKind
        , isWitnessKind
        , isAlgDataType
	, isWitnessType)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as T


-- Atoms ----------------------------------------------------------------------
-- | Test if some type is an empty TSum
isBot :: Type n -> Bool
isBot tt
        | TSum ss       <- tt
        , []            <- T.toList ss
        = True
        
        | otherwise     = False


-- | Check whether a type is a `TVar`, `TCon` or is Bottom.
isAtomT :: Type n -> Bool
isAtomT tt
 = case tt of
        TVar{}          -> True
        TCon{}          -> True
        _               -> isBot tt


-- Kinds ----------------------------------------------------------------------
-- | Check if some kind is the data kind.
isDataKind :: Kind n -> Bool
isDataKind tt
 = case tt of
        TCon (TyConKind KiConData)    -> True
        _                             -> False


-- | Check if some kind is the region kind.
isRegionKind :: Region n -> Bool
isRegionKind tt
 = case tt of
        TCon (TyConKind KiConRegion)  -> True
        _                             -> False


-- | Check if some kind is the effect kind.
isEffectKind :: Kind n -> Bool
isEffectKind tt
 = case tt of
        TCon (TyConKind KiConEffect)  -> True
        _                             -> False


-- | Check if some kind is the closure kind.
isClosureKind :: Kind n -> Bool
isClosureKind tt
 = case tt of
        TCon (TyConKind KiConClosure) -> True
        _                             -> False


-- | Check if some kind is the witness kind.
isWitnessKind :: Kind n -> Bool
isWitnessKind tt
 = case tt of
        TCon (TyConKind KiConWitness) -> True
        _                             -> False


-- Data Types -----------------------------------------------------------------
-- | Check whether this type is that of algebraic data.
--
--   It needs to have an explicit data constructor out the front,
--   and not a type variable. The constructor must not be the function
--   constructor, and must return a value of kind '*'.

-- Algebraic data types are all built from constructors
-- that have '*' as their result kind.
-- The function constructor (->) also has this result kind,
-- but it is in `TyConComp`, so is easy to ignore.
isAlgDataType :: Eq n => Type n -> Bool
isAlgDataType tt
        | Just (tc, _)  <- takeTyConApps tt
        , TyConBound u  <- tc
        = takeResultKind (typeOfBound u) == kData

        | otherwise
        = False

-- | Check whether type is a witness constructor
isWitnessType :: Eq n => Type n -> Bool
isWitnessType tt
 = case tt of
	TApp (TCon (TyConWitness _)) _ -> True
	_			   -> False
