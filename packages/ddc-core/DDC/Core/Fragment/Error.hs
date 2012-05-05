
module DDC.Core.Fragment.Error
        (Error(..))
where
import DDC.Core.Fragment.Feature
import DDC.Core.Exp
import DDC.Core.Pretty


data Error n
        = ErrorUnsupported      Feature
        | ErrorUndefinedPrim    n 
        | ErrorUndefinedVar     n
        | ErrorShadowedBind     n
        | ErrorUnusedBind       n
        | ErrorNakedType        (Type    n)
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
