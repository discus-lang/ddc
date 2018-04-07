{-# OPTIONS_HADDOCK hide #-}

-- | Predicates on type expressions.
module DDC.Source.Discus.Exp.Type.Predicates
        (isAtomT)
where
import DDC.Source.Discus.Exp.Type.Base


-- | Check whether a type is a `TVar` or `TCon`.
isAtomT :: GType l -> Bool
isAtomT tt
 = case tt of
        TAnnot _ t      -> isAtomT t
        TCon{}          -> True
        TVar{}          -> True
        _               -> False