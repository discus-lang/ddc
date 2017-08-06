{-# OPTIONS_HADDOCK hide #-}
-- | Errors produced when checking types.
module DDC.Core.Check.Error.ErrorTypeMessage where
import DDC.Core.Check.Error.ErrorType
import DDC.Type.Exp.Simple
import DDC.Type.Universe
import DDC.Data.Pretty


instance (Eq n, Show n, Pretty n)
       => Pretty (ErrorType n) where
 ppr = ppr'


-- Generic Problems -----------------------------------------------------------
ppr' (ErrorTypeUniverseMalfunction t u)
 = vcat [ text "Universe malfunction."
        , text "               Type: "  <> ppr t
        , text " is not in universe: "  <> ppr u ]

ppr' (ErrorTypeMismatch uni tInferred tExpected tt)
 = let (thing, thing')
        = case uni of
               UniverseSpec    -> ("Kind", "kind")
               UniverseKind    -> ("Sort", "sort")
               _               -> ("Type", "type")
   in vcat
       [ text thing <+> text "mismatch."
       , text "                Expected"
               <+> text thing' <> text ":"      <+> ppr tExpected
       , text " does not match inferred"
               <+> text thing' <> text ":"      <+> ppr tInferred
       , empty
       , text "with: "                          <> align (ppr tt) ]

ppr' (ErrorTypeInfinite tExt tBind)
 = vcat [ text "Cannot construct infinite type."
        , text "  " <> ppr tExt <+> text "=" <+> ppr tBind ]


-- Variables ------------------------------------------------------------------
ppr' (ErrorTypeUndefined u)
 = text "Undefined type variable: "     <> ppr u


-- Constructors ---------------------------------------------------------------
ppr' (ErrorTypeUnappliedKindFun)
 = text "Can't take sort of unapplied kind function constructor."

ppr' (ErrorTypeNakedSort s)
 = text "Can't check a naked sort: "    <> ppr s

ppr' (ErrorTypeUndefinedTypeCtor u)
 = text "Undefined type constructor: "  <> ppr u


-- Applications ---------------------------------------------------------------
ppr' (ErrorTypeAppNotFun tt t1 k1 t2)
 = vcat [ text "Type function used in application has invalid kind."
        , text "    In application: "           <> ppr tt
        , text " cannot apply type: "           <> ppr t1
        , text "           of kind: "           <> ppr k1
        , text "           to type: "           <> ppr t2 ]

ppr' (ErrorTypeAppArgMismatch tt tFn kFn tArg kArg)
 = vcat [ text "Kind mismatch in type application."
        , text "    In application: "           <> ppr tt
        , text " cannot apply type: "           <> ppr tFn
        , text "         with kind: "           <> ppr kFn
        , text "       to argument: "           <> ppr tArg
        , text "         with kind: "           <> ppr kArg ]


ppr' (ErrorTypeWitnessImplInvalid tt t1 k1 t2 k2)
 = vcat [ text "Invalid args for witness implication."
        , text "            left type: "        <> ppr t1
        , text "             has kind: "        <> ppr k1
        , text "           right type: "        <> ppr t2
        , text "             has kind: "        <> ppr k2
        , text "        when checking: "        <> ppr tt ]


-- Quantifiers ----------------------------------------------------------------
ppr' (ErrorTypeForallKindInvalid tt t k)
 = vcat [ text "Invalid kind for body of quantified type."
        , text "        the body type: "        <> ppr t
        , text "             has kind: "        <> ppr k
        , text "  but it must be Data or Prop"
        , text "        when checking: "        <> ppr tt ]


-- Sums -----------------------------------------------------------------------
ppr' (ErrorTypeSumKindMismatch k ts ks)
 = vcat
 $      [ text "Kind mismatch in type sum."
        , text " found multiple types: "        <> ppr ts
        , text " with differing kinds: "        <> ppr ks ]
 ++ (if k /= tBot sComp
                then [text "        expected kind: " <> ppr k ]
                else [])

ppr' (ErrorTypeSumKindInvalid ts k)
 = vcat [ text "Invalid kind for type sum."
        , text "         the type sum: "        <> ppr ts
        , text "             has kind: "        <> ppr k
        , text "  but it must be Effect or Closure" ]

