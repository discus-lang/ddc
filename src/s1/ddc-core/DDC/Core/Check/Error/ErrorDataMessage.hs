{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Error.ErrorDataMessage where
import DDC.Core.Check.Error.ErrorData
import DDC.Data.Pretty


instance (Eq n, Show n, Pretty n)
       => Pretty (ErrorData n) where
 ppr = ppr'

ppr' (ErrorDataDupTypeName n)
 = vcat [ text "Duplicate data type definition."
        , text "  A constructor with name: "    <> ppr n
        , text "  is already defined." ]

ppr' (ErrorDataDupCtorName n)
 = vcat [ text "Duplicate data constructor definition."
        , text "  A constructor with name: "    <> ppr n
        , text "  is already defined." ]


ppr' (ErrorDataWrongResult n tActual tExpected)
 = vcat [ text "Invalid result type for data constructor."
        , text "       The data constructor: "  <> ppr n
        , text "            has result type: "  <> ppr tActual
        , text "  but the enclosing type is: "  <> ppr tExpected ]

