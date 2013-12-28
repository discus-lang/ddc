
module DDC.Core.Tetra.Transform.Boxing
        (boxingModule)
where
import DDC.Core.Tetra.Compounds
import DDC.Core.Tetra.Prim
import DDC.Core.Transform.Boxing
import DDC.Core.Module
import DDC.Core.Exp


-- | Manage boxing of numeric values in a module.
boxingModule :: Module a Name -> Module a Name
boxingModule mm
        = boxing config mm


-- | Tetra-specific configuration for boxing transform.
config :: Config a Name
config  = Config
        { configTypeRep         = tetraTypeRep
        , configTypeBoxed       = tetraTypeBoxed
        , configTypeUnboxed     = tetraTypeUnboxed
        , configNameIsLiteral   = isNameLit
        , configBoxLiteral      = tetraBoxLiteral }



-- | Get the representation of some type.
tetraTypeRep :: Type Name -> Rep
tetraTypeRep
        = error "tetraTypeRep: not finished"


-- | Get the boxed version of some type.
tetraTypeBoxed :: Type Name -> Maybe (Type Name)
tetraTypeBoxed
        = error "tetraTypeBoxed: not finished"


-- | Get the unboxed version of some type.
tetraTypeUnboxed :: Type Name -> Maybe (Type Name)
tetraTypeUnboxed
        = error "tetraTypeUnboxed: not finished"


-- | Convert a literal name to a boxed representation.
tetraBoxLiteral :: a -> Name -> Maybe (Exp a Name)
tetraBoxLiteral a nLit
        | Just tLit    <- takeTypeOfLitName nLit
        , Just tBoxed  <- tetraTypeBoxed tLit
        = xCastConvert a 
                tBoxed tLit 
                (XVar (UPrim nLit) tLit)
