
module DDC.Core.Fragment.Error
        (Error(..))
where
import DDC.Core.Fragment.Feature
import DDC.Core.Exp
import DDC.Core.Pretty

-- | Language fragment compliance violations.
data Error n
        -- | Found an unsupported language feature.
        = ErrorUnsupported      Feature

        -- | Found an undefined primitive operator.
        | ErrorUndefinedPrim    n 

        -- | Found an unbound variable.
        | ErrorUndefinedVar     n

        -- | Found a variable binder that shadows another one at a higher scope,
        --   but the profile doesn't permit this.
        | ErrorShadowedBind     n

        -- | Found a bound variable with no uses,
        --   but the profile doesn't permit this.
        | ErrorUnusedBind       n

        -- | Found a naked type that isn't used as a function argument.
        | ErrorNakedType        (Type    n)

        -- | Found a naked witness that isn't used as a function argument.
        | ErrorNakedWitness     (Witness n)
        deriving (Eq, Show)


instance (Pretty n, Eq n) => Pretty (Error n) where
 ppr err
  = case err of
        ErrorUnsupported feature
         -> vcat [ text "Unsupported feature: " <> text (show feature) ]

        ErrorUndefinedPrim n
         -> vcat [ text "Undefined primitive: " <> ppr n ]

        ErrorUndefinedVar n
         -> vcat [ text "Undefined variable: " <> ppr n ]

        ErrorShadowedBind n
         -> vcat [ text "Binding shadows existing name: " <> ppr n ]

        ErrorUnusedBind n
         -> vcat [ text "Bound name is not used: " <> ppr n ]

        ErrorNakedType t
         -> vcat [ text "Naked type is not a function argument: " <> ppr t]

        ErrorNakedWitness w
         -> vcat [ text "Naked witness is not a function argument: " <> ppr w ]
