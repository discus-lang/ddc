
module DDC.Source.Tetra.Prim.OpStore
        (typeOpStore)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Exp


-- | Take the type of a primitive arithmetic operator.
typeOpStore :: OpStore -> Maybe (Type Name)
typeOpStore op
 = case op of
        _       -> Nothing
