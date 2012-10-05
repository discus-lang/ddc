
-- | Predicates on type expressions.
module DDC.Type.Predicates
        ( -- * Binders
          isBNone
        , isBAnon
        , isBName

          -- * Atoms
        , isBot
        , isAtomT

          -- * Kinds
        , isDataKind
        , isRegionKind
        , isEffectKind
        , isClosureKind
        , isWitnessKind

          -- * Data Types
        , isAlgDataType
        , isWitnessType
	    , isConstWitType
        , isMutableWitType
	    , isDistinctWitType)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as T


-- Binders --------------------------------------------------------------------
isBNone :: Bind n -> Bool
isBNone bb
 = case bb of
        BNone{} -> True
        _       -> False

isBAnon :: Bind n -> Bool
isBAnon bb
 = case bb of
        BAnon{} -> True
        _       -> False

isBName :: Bind n -> Bool
isBName bb
 = case bb of
        BName{} -> True
        _       -> False


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
---
--   The function constructor (->) also has this result kind,
--   but it is in `TyConComp`, so is easy to ignore.
isAlgDataType :: Eq n => Type n -> Bool
isAlgDataType tt
        | Just (tc, _)   <- takeTyConApps tt
        , TyConBound _ k <- tc
        = takeResultKind k == kData

        | otherwise
        = False

-- | Check whether type is a witness constructor
isWitnessType :: Eq n => Type n -> Bool
isWitnessType tt
 = case takeTyConApps tt of
	Just (TyConWitness _, _) -> True
	_			 -> False
	

isConstWitType :: Eq n => Type n -> Bool
isConstWitType tt
 = case takeTyConApps tt of
        Just (TyConWitness TwConConst, _) -> True
        _                                 -> False

isMutableWitType :: Eq n => Type n -> Bool
isMutableWitType tt
 = case takeTyConApps tt of
        Just (TyConWitness TwConMutable, _) -> True
        _                                   -> False

isDistinctWitType :: Eq n => Type n -> Bool
isDistinctWitType tt
 = case takeTyConApps tt of
        Just (TyConWitness (TwConDistinct _), _) -> True
        _                                        -> False
	
