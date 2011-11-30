
-- | Predicates on type expressions.
module DDC.Type.Predicates
        ( isBot
        , isBottom
        , isEffectKind
        , isClosureKind)
where
import DDC.Type.Exp
import qualified DDC.Type.Sum   as T

-- | Test if some type is a `TBot`. 
isBot :: Type n -> Bool
isBot (TBot _)  = True
isBot _         = False


-- | Test if some type is a `TBot` or an empty `TSum`.
isBottom :: Type n -> Bool
isBottom tt
        | TBot{}        <- tt = True

        | TSum ss       <- tt
        , []            <- T.toList ss
        = True
        
        | otherwise     = False


-- | Check if some kind is the effect kind.
isEffectKind :: Kind n -> Bool
isEffectKind tt
 = case tt of
        TCon (TyConKind (KiConEffect))  -> True
        _                               -> False


-- | Check if some kind is the closure kind.
isClosureKind :: Kind n -> Bool
isClosureKind tt
 = case tt of
        TCon (TyConKind (KiConClosure)) -> True
        _                               -> False

