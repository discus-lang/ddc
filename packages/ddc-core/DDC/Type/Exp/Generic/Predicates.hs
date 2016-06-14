
-- | Predicates on type expressions.
module DDC.Type.Exp.Generic.Predicates
        (isAtomT)
where
import DDC.Type.Exp.Generic.Exp


-- | Check whether a type is a `TVar` or `TCon`.
isAtomT :: GType l -> Bool
isAtomT tt
 = case tt of
        TAnnot _ t      -> isAtomT t
        TCon{}          -> True
        TVar{}          -> True
        _               -> False