
-- | Errors produced when checking types.
module DDC.Type.Check.ErrorMessage where
import DDC.Type.Check.Error
import DDC.Type.Universe
import DDC.Type.Compounds
import DDC.Type.Pretty


instance (Eq n, Show n, Pretty n) => Pretty (Error n) where
 ppr err
  = case err of
        -- Generic Problems ---------------------
        ErrorUniverseMalfunction t u
         -> vcat [ text "Universe malfunction."
                 , text "               Type: " <> ppr t
                 , text " is not in universe: " <> ppr u ]

        ErrorMismatch uni tInferred tExpected tt
         -> let (thing, thing')   
                 = case uni of
                        UniverseSpec    -> ("Kind", "kind")
                        UniverseKind    -> ("Sort", "sort")
                        _               -> ("Type", "type")
            in vcat 
                [ text thing <+> text "mismatch."
                , text "                Expected"
                        <+> text thing' <> text ":"    <+> ppr tExpected
                , text " does not match inferred"
                        <+> text thing' <> text ":"    <+> ppr tInferred
                , empty
                , text "with: "                         <> align (ppr tt) ]

        ErrorInfinite tExt tBind
         -> vcat [ text "Cannot construct infinite type."
                 , text "  " <> ppr tExt <+> text "=" <+> ppr tBind ]

        -- Variables ----------------------------
        ErrorUndefined u
         -> text "Undefined type variable: " <> ppr u


        -- Constructors -------------------------
        ErrorUnappliedKindFun 
         -> text "Can't take sort of unapplied kind function constructor."

        ErrorNakedSort s
         -> text "Can't check a naked sort: " <> ppr s
                
        ErrorUndefinedTypeCtor u
         -> text "Undefined type constructor: " <> ppr u


        -- Applications -------------------------
        ErrorAppNotFun tt t1 k1 t2
         -> vcat [ text "Type function used in application has invalid kind."
                 , text "    In application: "          <> ppr tt
                 , text " cannot apply type: "          <> ppr t1
                 , text "           of kind: "          <> ppr k1
                 , text "           to type: "          <> ppr t2 ]
 
        ErrorAppArgMismatch tt tFn kFn tArg kArg
         -> vcat [ text "Kind mismatch in type application."
                 , text "    In application: "          <> ppr tt
                 , text " cannot apply type: "          <> ppr tFn
                 , text "         with kind: "          <> ppr kFn
                 , text "       to argument: "          <> ppr tArg
                 , text "         with kind: "          <> ppr kArg ]         


        ErrorWitnessImplInvalid tt t1 k1 t2 k2
         -> vcat [ text "Invalid args for witness implication."
                 , text "            left type: " <> ppr t1
                 , text "             has kind: " <> ppr k1
                 , text "           right type: " <> ppr t2
                 , text "             has kind: " <> ppr k2 
                 , text "        when checking: " <> ppr tt ]


        -- Quantifiers --------------------------
        ErrorForallKindInvalid tt t k
         -> vcat [ text "Invalid kind for body of quantified type."
                 , text "        the body type: " <> ppr t
                 , text "             has kind: " <> ppr k
                 , text "  but it must be Data or Prop" 
                 , text "        when checking: " <> ppr tt ]


        -- Sums ---------------------------------                    
        ErrorSumKindMismatch k ts ks
         -> vcat $ [ text "Kind mismatch in type sum."
                 , text " found multiple types: " <> ppr ts
                 , text " with differing kinds: " <> ppr ks ]
                 ++ (if k /= tBot sComp
                        then [text "        expected kind: " <> ppr k ]
                        else [])
                
        ErrorSumKindInvalid ts k
         -> vcat [ text "Invalid kind for type sum."
                 , text "         the type sum: " <> ppr ts
                 , text "             has kind: " <> ppr k
                 , text "  but it must be Effect or Closure" ]


instance (Eq n, Show n, Pretty n) => Pretty (ErrorData n) where
 ppr err
  = case err of
        ErrorDataDupTypeName n
         -> vcat [ text "Duplicate data type definition."
                 , text "  A constructor with name: " <> ppr n
                 , text "  is already defined." ]

        ErrorDataDupCtorName n
         -> vcat [ text "Duplicate data constructor definition."
                 , text "  A constructor with name: " <> ppr n
                 , text "  is already defined." ]


        ErrorDataWrongResult n tActual tExpected
         -> vcat [ text "Invalid result type for data constructor."
                 , text "       The data constructor: " <> ppr n
                 , text "            has result type: " <> ppr tActual
                 , text "  but the enclosing type is: " <> ppr tExpected ]

