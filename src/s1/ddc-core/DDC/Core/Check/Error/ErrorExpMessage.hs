
-- | Errors produced when checking core expressions.
module DDC.Core.Check.Error.ErrorExpMessage
        (Error(..))
where
import DDC.Core.Pretty
import DDC.Core.Check.Error.ErrorExp
import DDC.Core.Check.Error.ErrorTypeMessage      ()
import DDC.Core.Check.Error.ErrorDataMessage      ()
import DDC.Type.Exp.Simple
import DDC.Type.Universe


instance (Pretty a, Show n, Eq n, Pretty n)
       => Pretty (Error a n) where
 ppr = ppr'


-- Wrapped Errors -------------------------------------------------------------
ppr' (ErrorType err')
        = ppr err'

ppr' (ErrorData err')
        = ppr err'

-- Modules --------------------------------------------------------------------
ppr' (ErrorExportUndefined n)
 = vcat [ text "Exported name '" <> ppr n <> text "' is undefined." ]

ppr' (ErrorExportDuplicate n)
 = vcat [ text "Duplicate exported name '" <> ppr n <> text "'."]

ppr' (ErrorExportMismatch n tExport tDef)
 = vcat [ text "Type of exported name does not match type of definition."
        , text "             with binding: "   <> ppr n
        , text "           type of export: "   <> ppr tExport
        , text "       type of definition: "   <> ppr tDef ]

ppr' (ErrorImportDuplicate n)
 = vcat [ text "Duplicate imported name '" <> ppr n <> text "'."]

ppr' (ErrorImportCapNotEffect n)
 = vcat [ text "Imported capability '"
                <> ppr n
                <> text "' does not have kind Effect." ]

ppr' (ErrorImportValueNotData n)
 = vcat [ text "Imported value '"
                <> ppr n
                <> text "' does not have kind Data." ]


-- Exp -------------------------------------------------------------------------
ppr' (ErrorMismatch a tInferred tExpected _xx)
 = vcat [ ppr a
        , text "Type mismatch."
        , text "  inferred type: " <> ppr tInferred
        , text "  expected type: " <> ppr tExpected ]


-- Variable -------------------------------------------------------------------
ppr' (ErrorUndefinedVar a u universe)
 = case universe of
        UniverseSpec
          -> vcat  [ ppr a
                   , text "Undefined spec variable: "      <> ppr u ]

        UniverseData
          -> vcat  [ ppr a
                   , text "Undefined value variable: "     <> ppr u ]

        UniverseWitness
          -> vcat  [ ppr a
                   , text "Undefined witness variable: "   <> ppr u ]

        -- Universes other than the above don't have variables,
        -- but let's not worry about that here.
        _ -> vcat  [ ppr a
                   , text "Undefined variable: "           <> ppr u ]


-- Constructor ----------------------------------------------------------------
ppr' (ErrorUndefinedCtor a xx)
 = vcat [ ppr a
        , text "Undefined data constructor: "  <> ppr xx ]


-- Application ----------------------------------------------------------------
ppr' (ErrorAppNotFun a _xx t1)
 = vcat [ ppr a
        , text "Cannot apply non-function"
        , text "              of type: "       <> ppr t1 ]

ppr' (ErrorAppCannotInferPolymorphic a _xx)
 = vcat [ ppr a
        , text "Cannot infer the type of a polymorphic expression."
        , text "  Please supply type annotations to constrain the functional"
        , text "  part to have a quantified type." ]


-- Lambda ---------------------------------------------------------------------
ppr' (ErrorAbsShadow a _xx b)
 = vcat [ ppr a
        , text "Cannot shadow variable."
        , text "  binder: "                    <> ppr b
        , text "  is already in the environment." ]

ppr' (ErrorAbsParamUnannotated a b1)
 = vcat [ ppr a
        , text "Missing annotation on function parameter."
        , text "  With paramter: " <> ppr b1 ]

ppr' (ErrorAbsNotPure a _xx universe eff)
 = vcat [ ppr a
        , text "Impure" <+> ppr universe <+> text "abstraction"
        , text "  has effect: "       <> ppr eff ]

ppr' (ErrorAbsBindBadKind a _xx t1 k1)
 = vcat [ ppr a
        , text "Function parameter has invalid kind."
        , text "    The function parameter: "   <> ppr t1
        , text "                  has kind: "  <> ppr k1
        , text "            but it must be: Data or Witness" ]


-- Letrec ---------------------------------------------------------------------
ppr' (ErrorLetrecRebound a _xx b)
 = vcat [ ppr a
        , text "Redefined binder '" <> ppr b <> text "' in letrec." ]

ppr' (ErrorLetrecMissingAnnot a b _xx)
 = vcat [ ppr a
        , text "Missing or incomplete type annotation on recursive let-binding '"
               <> ppr (binderOfBind b) <> text "'."
        , text "Recursive functions must have full type annotations." ]

ppr' (ErrorLetrecBindingNotLambda a _xx x)
 = vcat [ ppr a
        , text "Letrec can only bind lambda abstractions."
        , text "      This is not one: "       <> ppr x ]


-- Letregion ------------------------------------------------------------------
-- TODO: The concrete syntax is such that we cannot cause this error
-- unless there is a bug in the compiler.
ppr' (ErrorPrivateNotRegion a _xx bs ks)
 = vcat [ ppr a
        , text "Letregion binders do not have region kind."
        , text "        Region binders: "       <> (hcat $ map ppr bs)
        , text "             has kinds: "       <> (hcat $ map ppr ks)
        , text "       but they must all be: Region" ]

ppr' (ErrorPrivateRebound a _xx bs)
 = vcat [ ppr a
        , text "Region variables shadow existing ones."
        , text "           Region variables: "  <> (hcat $ map ppr bs)
        , text "     are already in environment" ]

ppr' (ErrorPrivateEscape a _xx bs t)
 = vcat [ ppr a
        , text "Region variables escape scope of private."
        , text "       The region variables: "  <> (hcat $ map ppr bs)
        , text "   is free in the body type: "   <> ppr t ]

ppr' (ErrorPrivateWitnessInvalid a _xx b)
 = vcat [ ppr a
        , text "Invalid witness type with private."
        , text "          The witness: "       <> ppr b
        , text "  cannot be created with a private" ]

ppr' (ErrorPrivateWitnessConflict a _xx b1 b2)
 = vcat [ ppr a
        , text "Conflicting witness types with private."
        , text "      Witness binding: "       <> ppr b1
        , text "       conflicts with: "       <> ppr b2 ]

ppr' (ErrorPrivateWitnessOther a _xx bs1 b2)
 = vcat [ ppr a
        , text "Witness type is not for bound regions."
        , text "        private binds: "       <> (hsep $ map ppr bs1)
        , text "  but witness type is: "       <> ppr b2 ]


-- Witnesses ------------------------------------------------------------------
ppr' (ErrorWAppMismatch a ww t1 t2)
 = vcat [ ppr a
        , text "Type mismatch in witness application."
        , text "  Constructor expects: "       <> ppr t1
        , text "      but argument is: "       <> ppr t2
        , empty
        , text "with: "                        <> align (ppr ww) ]

ppr' (ErrorWAppNotCtor a ww t1 t2)
 = vcat [ ppr a
        , text "Type cannot apply non-constructor witness"
        , text "              of type: "       <> ppr t1
        , text "  to argument of type: "       <> ppr t2
        , empty
        , text "with: "                        <> align (ppr ww) ]

ppr' (ErrorWitnessNotPurity a xx w t)
 = vcat [ ppr a
        , text "Witness for a purify does not witness purity."
        , text "        Witness: "             <> ppr w
        , text "       has type: "             <> ppr t
        , empty
        , text "with: "                        <> align (ppr xx) ]


-- Case Expressions -----------------------------------------------------------
ppr' (ErrorCaseScrutineeNotAlgebraic a xx tScrutinee)
 = vcat [ ppr a
        , text "Scrutinee of case expression is not algebraic data."
        , text "     Scrutinee type: "         <> ppr tScrutinee
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseScrutineeTypeUndeclared a xx tScrutinee)
 = vcat [ ppr a
        , text "Type of scrutinee does not have a data declaration."
        , text "     Scrutinee type: "         <> ppr tScrutinee
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseNoAlternatives a xx)
 = vcat [ ppr a
        , text "Case expression does not have any alternatives."
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseNonExhaustive a xx ns)
 = vcat [ ppr a
        , text "Case alternatives are non-exhaustive."
        , text " Constructors not matched: "
               <> (sep $ punctuate comma $ map ppr ns)
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseNonExhaustiveLarge a xx)
 = vcat [ ppr a
        , text "Case alternatives are non-exhaustive."
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseOverlapping a xx)
 = vcat [ ppr a
        , text "Case alternatives are overlapping."
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseTooManyBinders a xx uCtor iCtorFields iPatternFields)
 = vcat [ ppr a
        , text "Pattern has more binders than there are fields in the constructor."
        , text "     Contructor: " <> ppr uCtor
        , text "            has: " <> ppr iCtorFields
                                   <+> text "fields"
        , text "  but there are: " <> ppr iPatternFields
                                  <+> text "binders in the pattern"
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseCannotInstantiate a xx tScrutinee tCtor)
 = vcat [ ppr a
        , text "Cannot instantiate constructor type with scrutinee type args."
        , text " Either the constructor has an invalid type,"
        , text " or the type of the scrutinee does not match the type of the pattern."
        , text "        Scrutinee type: "      <> ppr tScrutinee
        , text "      Constructor type: "      <> ppr tCtor
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseScrutineeTypeMismatch a xx tScrutinee tPattern)
 = vcat [ ppr a
        , text "Scrutinee type does not match result of pattern type."
        , text "        Scrutinee type: "      <> ppr tScrutinee
        , text "          Pattern type: "      <> ppr tPattern
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseFieldTypeMismatch a xx tAnnot tField)
 = vcat [ ppr a
        , text "Annotation on pattern variable does not match type of field."
        , text "       Annotation type: "      <> ppr tAnnot
        , text "            Field type: "      <> ppr tField
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorCaseAltResultMismatch a xx t1 t2)
 = vcat [ ppr a
        , text "Mismatch in alternative result types."
        , text "   Type of alternative: "      <> ppr t1
        , text "        does not match: "      <> ppr t2
        , empty
        , text "with: "                        <> align (ppr xx) ]


-- Casts ----------------------------------------------------------------------
ppr' (ErrorWeakEffNotEff a xx eff k)
 = vcat [ ppr a
        , text "Type provided for a 'weakeff' does not have effect kind."
        , text "           Type: "             <> ppr eff
        , text "       has kind: "             <> ppr k
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorRunNotSuspension a xx t)
 = vcat [ ppr a
        , text "Expression to run is not a suspension."
        , text "          Type: "              <> ppr t
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorRunNotSupported a xx eff)
 = vcat [ ppr a
        , text "Effect of computation not supported by context."
        , text "    Effect:  "                 <> ppr eff
        , empty
        , text "with: "                        <> align (ppr xx) ]

ppr' (ErrorRunCannotInfer a xx)
 = vcat [ ppr a
        , text "Cannot infer type of suspended computation."
        , empty
        , text "with: "                        <> align (ppr xx) ]


-- Type -----------------------------------------------------------------------
ppr' (ErrorNakedType a xx)
 = vcat [ ppr a
        , text "Found naked type in core program."
        , empty
        , text "with: "                        <> align (ppr xx) ]


-- Witness --------------------------------------------------------------------
ppr' (ErrorNakedWitness a xx)
 = vcat [ ppr a
        , text "Found naked witness in core program."
        , empty
        , text "with: "                        <> align (ppr xx) ]

