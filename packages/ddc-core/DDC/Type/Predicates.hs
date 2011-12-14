
-- | Predicates on type expressions.
module DDC.Type.Predicates
        ( isBot
        , isDataKind
        , isRegionKind
        , isEffectKind
        , isClosureKind)
where
import DDC.Type.Exp
import qualified DDC.Type.Sum   as T


-- | Test if some type is an empty TSum
isBot :: Type n -> Bool
isBot tt
        | TSum ss       <- tt
        , []            <- T.toList ss
        = True
        
        | otherwise     = False


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

