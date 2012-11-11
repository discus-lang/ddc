
-- | Predicates on type expressions.
module DDC.Type.Predicates
        ( -- * Binders
          isBNone
        , isBAnon
        , isBName

          -- * Atoms
        , isTVar
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
        , isDistinctWitType

          -- * Effect Types
        , isReadEffect
        , isWriteEffect
        , isAllocEffect
        , isSomeReadEffect
        , isSomeWriteEffect
        , isSomeAllocEffect)
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
-- | Check whether a type is a `TVar`
isTVar :: Type n -> Bool
isTVar tt
 = case tt of
        TVar{}          -> True
        _               -> False

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
	

-- | Check whether this is the type of a @Const@ witness.
isConstWitType :: Eq n => Type n -> Bool
isConstWitType tt
 = case takeTyConApps tt of
        Just (TyConWitness TwConConst, _) -> True
        _                                 -> False


-- | Check whether this is the type of a @Mutable@ witness.
isMutableWitType :: Eq n => Type n -> Bool
isMutableWitType tt
 = case takeTyConApps tt of
        Just (TyConWitness TwConMutable, _) -> True
        _                                   -> False


-- | Check whether this is the type of a @Distinct@ witness.
isDistinctWitType :: Eq n => Type n -> Bool
isDistinctWitType tt
 = case takeTyConApps tt of
        Just (TyConWitness (TwConDistinct _), _) -> True
        _                                        -> False
	

-- Effects --------------------------------------------------------------------
-- | Check whether this is an atomic read effect.
isReadEffect :: Effect n -> Bool
isReadEffect eff
 = case eff of
        TApp (TCon (TyConSpec TcConRead)) _     -> True
        _                                       -> False


-- | Check whether this is an atomic write effect.
isWriteEffect :: Effect n -> Bool
isWriteEffect eff
 = case eff of
        TApp (TCon (TyConSpec TcConWrite)) _    -> True
        _                                       -> False


-- | Check whether this is an atomic alloc effect.
isAllocEffect :: Effect n -> Bool
isAllocEffect eff
 = case eff of
        TApp (TCon (TyConSpec TcConAlloc)) _    -> True
        _                                       -> False


-- | Check whether an effect is some sort of read effect.
--   Matches @Read@ @HeadRead@ and @DeepRead@.
isSomeReadEffect :: Effect n -> Bool
isSomeReadEffect tt
 = case tt of
        TApp (TCon (TyConSpec con)) _
         -> case con of
                TcConRead       -> True
                TcConHeadRead   -> True
                TcConDeepRead   -> True
                _               -> False

        _                       -> False


-- | Check whether an effect is some sort of allocation effect.
--   Matches @Alloc@ and @DeepAlloc@
isSomeWriteEffect :: Effect n -> Bool
isSomeWriteEffect tt
 = case tt of
        TApp (TCon (TyConSpec con)) _
         -> case con of
                TcConWrite      -> True
                TcConDeepWrite  -> True
                _               -> False

        _                       -> False


-- | Check whether an effect is some sort of allocation effect.
--   Matches @Alloc@ and @DeepAlloc@
isSomeAllocEffect :: Effect n -> Bool
isSomeAllocEffect tt
 = case tt of
        TApp (TCon (TyConSpec con)) _
         -> case con of
                TcConAlloc      -> True
                TcConDeepAlloc  -> True
                _               -> False

        _                       -> False

