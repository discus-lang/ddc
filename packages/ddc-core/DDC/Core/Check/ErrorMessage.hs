{-# OPTIONS_HADDOCK hide #-}
-- | Errors produced when checking core expressions.
module DDC.Core.Check.ErrorMessage
        (Error(..))
where
import DDC.Core.Pretty
import DDC.Core.Check.Error
import DDC.Type.Compounds


instance (Pretty n, Eq n) => Pretty (Error a n) where
 ppr err
  = case err of
        ErrorType err'  -> ppr err'

        ErrorMalformedExp xx
         -> vcat [ text "Malformed expression: "        <> align (ppr xx) ]
        
        ErrorMalformedType xx tt
         -> vcat [ text "Found malformed type: "        <> ppr tt
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorNakedType xx
         -> vcat [ text "Found naked type in core program."
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorNakedWitness xx
         -> vcat [ text "Found naked witness in core program."
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        -- Variable ---------------------------------------
        ErrorVarAnnotMismatch u t
         -> vcat [ text "Type mismatch in annotation."
                 , text "             Variable: "       <> ppr u
                 , text "       has annotation: "       <> (ppr $ typeOfBound u)
                 , text " which conflicts with: "       <> ppr t
                 , text "     from environment." ]


        -- Application ------------------------------------
        ErrorAppMismatch xx t1 t2
         -> vcat [ text "Type mismatch in application." 
                 , text "     Function expects: "       <> ppr t1
                 , text "      but argument is: "       <> ppr t2
                 , empty
                 , text "with: "                        <> align (ppr xx) ]
         
        ErrorAppNotFun xx t1 t2
         -> vcat [ text "Cannot apply non-function"
                 , text "              of type: "       <> ppr t1
                 , text "  to argument of type: "       <> ppr t2 
                 , empty
                 , text "with: "                        <> align (ppr xx) ]


        -- Lambda -----------------------------------------
        ErrorLamNotPure xx eff
         -> vcat [ text "Impure type abstraction"
                 , text "           has effect: "       <> ppr eff
                 , empty
                 , text "with: "                        <> align (ppr xx) ]
                 
        
        ErrorLamBindNotData xx t1 k1
         -> vcat [ text "Function parameter does not have data kind."
                 , text "    The function parameter:"   <> ppr t1
                 , text "                  has kind: "  <> ppr k1
                 , text "            but it must be: *"
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLamBodyNotData xx b1 t2 k2
         -> vcat [ text "Result of function does not have data kind."
                 , text "   In function with binder: "  <> ppr b1
                 , text "       the result has type: "  <> ppr t2
                 , text "                 with kind: "  <> ppr k2
                 , text "            but it must be: *"
                 , empty
                 , text "with: "                        <> align (ppr xx) ]


        -- Let --------------------------------------------
        ErrorLetMismatch xx b t
         -> vcat [ text "Type mismatch in let-binding."
                 , text "                The binder: "  <> ppr (binderOfBind b)
                 , text "                  has type: "  <> ppr (typeOfBind b)
                 , text "     but the body has type: "  <> ppr t
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetBindingNotData xx b k
         -> vcat [ text "Let binding does not have data kind."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "             has type: "       <> ppr (typeOfBind b)
                 , text "            with kind: "       <> ppr k
                 , text "       but it must be: * "
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetBodyNotData xx t k
         -> vcat [ text "Let body does not have data kind."
                 , text " Body of let has type: "       <> ppr t
                 , text "            with kind: "       <> ppr k
                 , text "       but it must be: * "
                 , empty
                 , text "with: "                        <> align (ppr xx) ]


        -- Let Lazy ---------------------------------------
        ErrorLetLazyNotEmpty xx b clo
         -> vcat [ text "Lazy let binding is not empty."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "          has closure: "       <> ppr clo
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetLazyNotPure xx b eff
         -> vcat [ text "Lazy let binding is not pure."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "           has effect: "       <> ppr eff
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetLazyNoWitness xx b t
         -> vcat [ text "Lazy let binding has no witness but the bound value may have a head region."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "             Has type: "       <> ppr t
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetLazyWitnessTypeMismatch xx b tWitGot tBind tWitExp
         -> vcat [ text "Unexpected witness type in lazy let binding."
                 , text "          The binding for: "   <> ppr (binderOfBind b)
                 , text "    has a witness of type: "   <> ppr tWitGot
                 , text "           but is type is: "   <> ppr tBind
                 , text " so the witness should be: "   <> ppr tWitExp 
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        -- Letrec -----------------------------------------
        ErrorLetrecBindingNotLambda xx x
         -> vcat [ text "Letrec can only bind lambda abstractions."
                 , text "      This is not one: "       <> ppr x
                 , empty
                 , text "with: "                        <> align (ppr xx) ]


        -- Letregion --------------------------------------
        ErrorLetRegionNotRegion xx b k
         -> vcat [ text "Letregion binder does not have region kind."
                 , text "        Region binder: "       <> ppr b
                 , text "             has kind: "       <> ppr k
                 , text "       but is must be: %" 
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetRegionRebound xx b
         -> vcat [ text "Region variable shadows existing one."
                 , text "           Region variable: "  <> ppr b
                 , text "     is already in environment"
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetRegionFree xx b t
         -> vcat [ text "Region variable escapes scope of letregion."
                 , text "       The region variable: "  <> ppr b
                 , text "  is free in the body type: "  <> ppr t
                 , empty
                 , text "with: "                        <> align (ppr xx) ]
        
        ErrorLetRegionWitnessInvalid xx b
         -> vcat [ text "Invalid witness type with letregion."
                 , text "          The witness: "       <> ppr b
                 , text "  cannot be created with a letregion"
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetRegionWitnessConflict xx b1 b2
         -> vcat [ text "Conflicting witness types with letregion."
                 , text "      Witness binding: "       <> ppr b1
                 , text "       conflicts with: "       <> ppr b2 
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorLetRegionWitnessOther xx b1 b2
         -> vcat [ text "Witness type is not for bound region."
                 , text "      letregion binds: "       <> ppr b1
                 , text "  but witness type is: "       <> ppr b2
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorWithRegionNotRegion xx u k
         -> vcat [ text "Withregion handle does not have region kind."
                 , text "   Region var or ctor: "       <> ppr u
                 , text "             has kind: "       <> ppr k
                 , text "       but it must be: %"
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        -- Witnesses --------------------------------------
        ErrorWAppMismatch ww t1 t2
         -> vcat [ text "Type mismatch in witness application."
                 , text "  Constructor expects: "       <> ppr t1
                 , text "      but argument is: "       <> ppr t2
                 , empty
                 , text "with: "                        <> align (ppr ww) ]

        ErrorWAppNotCtor ww t1 t2
         -> vcat [ text "Type cannot apply non-constructor witness"
                 , text "              of type: "       <> ppr t1
                 , text "  to argument of type: "       <> ppr t2
                 , empty
                 , text "with: "                        <> align (ppr ww) ]

        ErrorCannotJoin ww w1 t1 w2 t2
         -> vcat [ text "Cannot join witnesses."
                 , text "          Cannot join: "       <> ppr w1
                 , text "              of type: "       <> ppr t1
                 , text "         with witness: "       <> ppr w2
                 , text "              of type: "       <> ppr t2
                 , empty
                 , text "with: "                        <> align (ppr ww) ]

        ErrorWitnessNotPurity xx w t
         -> vcat [ text "Witness for a purify does not witness purity."
                 , text "        Witness: "             <> ppr w
                 , text "       has type: "             <> ppr t
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorWitnessNotEmpty xx w t
         -> vcat [ text "Witness for a forget does not witness emptiness."
                 , text "        Witness: "             <> ppr w
                 , text "       has type: "             <> ppr t
                 , empty
                 , text "with: "                        <> align (ppr xx) ]


        -- Case Expressions -------------------------------
        ErrorCaseDiscrimNotAlgebraic xx tDiscrim
         -> vcat [ text "Discriminant of case expression is not algebraic data."
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , empty
                 , text "with: "                        <> align (ppr xx) ]
        
        ErrorCaseDiscrimTypeUndeclared xx tDiscrim
         -> vcat [ text "Type of discriminant does not have a data declaration."
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseNoAlternatives xx
         -> vcat [ text "Case expression does not have any alternatives."
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseNonExhaustive xx ns
         -> vcat [ text "Case alternatives are non-exhaustive."
                 , text " Constructors not matched: "   
                        <> (sep $ punctuate comma $ map ppr ns)
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseNonExhaustiveLarge xx
         -> vcat [ text "Case alternatives are non-exhaustive."
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseOverlapping xx
         -> vcat [ text "Case alternatives are overlapping."
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseTooManyBinders xx uCtor iCtorFields iPatternFields
         -> vcat [ text "Pattern has more binders than there are fields in the constructor."
                 , text "     Contructor: " <> ppr uCtor
                 , text "            has: " <> ppr iCtorFields      
                                            <+> text "fields"
                 , text "  but there are: " <> ppr iPatternFields   
                                           <+> text "binders in the pattern" 
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseCannotInstantiate xx tCtor tDiscrim
         -> vcat [ text "Cannot instantiate constructor type with discriminant type args."
                 , text " Either the constructor has an invalid type,"
                 , text " or the type of the discriminant does not match the type of the pattern."
                 , text "      Constructor type: "      <> ppr tCtor
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseDiscrimTypeMismatch xx tDiscrim tPattern
         -> vcat [ text "Discriminant type does not match result of pattern type."
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , text "          Pattern type: "      <> ppr tPattern
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseFieldTypeMismatch xx tAnnot tField
         -> vcat [ text "Annotation on pattern variable does not match type of field."
                 , text "       Annotation type: "      <> ppr tAnnot
                 , text "            Field type: "      <> ppr tField
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorCaseAltResultMismatch xx t1 t2
         -> vcat [ text "Mismatch in alternative result types."
                 , text "   Type of alternative: "      <> ppr t1
                 , text "        does not match: "      <> ppr t2
                 , empty
                 , text "with: "                        <> align (ppr xx) ]


        -- Casts ------------------------------------------
        ErrorMaxeffNotEff xx eff k
         -> vcat [ text "Type provided for a 'maxeff' does not have effect kind."
                 , text "           Type: "             <> ppr eff
                 , text "       has kind: "             <> ppr k
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorMaxcloNotClo xx clo k
         -> vcat [ text "Type provided for a 'maxclo' does not have closure kind."
                 , text "           Type: "             <> ppr clo
                 , text "       has kind: "             <> ppr k
                 , empty
                 , text "with: "                        <> align (ppr xx) ]

        ErrorMaxcloMalformed xx clo
         -> vcat [ text "Type provided for a 'maxclo' is malformed."
                 , text "        Closure: "             <> ppr clo
                 , text " can only contain 'Use' terms."
                 , empty
                 , text "with: "                        <> align (ppr xx) ]
       
