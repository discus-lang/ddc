{-# OPTIONS_HADDOCK hide #-}
-- | Errors produced when checking core expressions.
module DDC.Core.Check.CheckError
        (Error(..))
where
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Type.Compounds
import qualified DDC.Type.Check as T


-- | Type errors.
data Error a n
        -- | Found a kind error when checking a type.
        = ErrorType
        { errorTypeError        :: T.Error n }

        -- | Found a malformed exp, and we don't have a more specific diagnosis.
        | ErrorMalformedExp
        { errorChecking         :: Exp a n }

        -- | Found a malformed type, and we don't have a more specific diagnosis.
        | ErrorMalformedType
        { errorChecking         :: Exp a n
        , errorType             :: Type n }

        -- Var --------------------------------------------
        -- | Type in environment does not match type annotation on variable.
        | ErrorVarAnnotMismatch
        { errorBound            :: Bound n
        , errorTypeEnv          :: Type n }

        -- Application ------------------------------------
        -- | Types of parameter and arg don't match when checking application.
        | ErrorAppMismatch
        { errorChecking         :: Exp a n
        , errorParamType        :: Type n
        , errorArgType          :: Type n }

        -- | Tried to apply a non function to an argument.
        | ErrorAppNotFun
        { errorChecking         :: Exp a n
        , errorNotFunType       :: Type n
        , errorArgType          :: Type n }


        -- Lambda -----------------------------------------
        -- | Non-computation abstractions cannot have visible effects.
        | ErrorLamNotPure
        { errorChecking         :: Exp a n
        , errorEffect           :: Effect n }

        -- | Computation lambdas must bind values of data kind.
        | ErrorLamBindNotData
        { errorChecking         :: Exp a n 
        , errorType             :: Type n
        , errorKind             :: Kind n }

        -- | The body of Spec and Witness lambdas must be of data kind.
        | ErrorLamBodyNotData
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n
        , errorKind             :: Kind n }
        
        -- | Tried to shadow a level-1 binder.
        | ErrorLamReboundSpec
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n }


        -- Let --------------------------------------------
        -- | In let expression, type of binder does not match type of right of binding.
        | ErrorLetMismatch
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }

        -- | Let(rec) bindings should have kind '*'
        | ErrorLetBindingNotData
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorKind             :: Kind n }

        -- | Let(rec,region,withregion) body should have kind '*'
        | ErrorLetBodyNotData
        { errorChecking         :: Exp a n
        , errorType             :: Type n
        , errorKind             :: Kind n }


        -- Let Lazy ---------------------------------------
        -- | Lazy let binding is not pure.
        | ErrorLetLazyNotPure
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorEffect           :: Effect n }

        -- | Lazy let binding is not empty.
        | ErrorLetLazyNotEmpty
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorClosure          :: Closure n }

        -- | Lazy let binding has no Lazy witness, but the type of the binding
        --   has a head region.
        | ErrorLetLazyNoWitness
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }

        -- | Witness provided to lazy let binding has the wrong type.
        | ErrorLetLazyWitnessTypeMismatch 
        { errorChecking          :: Exp a n
        , errorBind              :: Bind n
        , errorWitnessTypeHave   :: Type n
        , errorBindType          :: Type n
        , errorWitnessTypeExpect :: Type n }


        -- Letrec -----------------------------------------
        -- | Letrec bindings must be syntactic lambdas.
        | ErrorLetrecBindingNotLambda
        { errorChecking         :: Exp a n 
        , errorExp              :: Exp a n }


        -- Letregion --------------------------------------
        -- | Region binding does not have region kind.
        | ErrorLetRegionNotRegion
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorKind             :: Kind n }

        -- | Tried to rebind a region variable with the same name as on in the environment.
        | ErrorLetRegionRebound
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n }

        -- | Bound region variable is free in the type of the body of a letregion.
        | ErrorLetRegionFree
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }

        -- | A witness with this type cannot be created at a letregion.
        | ErrorLetRegionWitnessInvalid
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n }

        -- | A witness conflicts with another one defined with the same letregion.
        | ErrorLetRegionWitnessConflict
        { errorChecking         :: Exp a n
        , errorBindWitness1     :: Bind n
        , errorBindWitness2     :: Bind n }

        -- | A witness introduced with a letregion was for some other region.
        | ErrorLetRegionWitnessOther
        { errorChecking         :: Exp a n
        , errorBoundRegion      :: Bound n
        , errorBindWitness      :: Bind  n }

        -- | Withregion handle does not have region kind.
        | ErrorWithRegionNotRegion
        { errorChecking         :: Exp a n
        , errorBound            :: Bound n
        , errorKind             :: Kind n }


        -- Witnesses --------------------------------------
        -- | Type mismatch in witness application.
        | ErrorWAppMismatch
        { errorWitness          :: Witness n
        , errorParamType        :: Type n
        , errorArgType          :: Type n }

        -- | Cannot apply a non-constructor witness.
        | ErrorWAppNotCtor
        { errorWitness          :: Witness n
        , errorNotFunType       :: Type n
        , errorArgType          :: Type n }

        -- | Cannot join witnesses.
        | ErrorCannotJoin
        { errorWitness          :: Witness n
        , errorWitnessLeft      :: Witness n
        , errorTypeLeft         :: Type n
        , errorWitnessRight     :: Witness n
        , errorTypeRight        :: Type n }

        -- | Witness provided for a purify does not witness purity.
        | ErrorWitnessNotPurity
        { errorChecking         :: Exp a n
        , errorWitness          :: Witness n
        , errorType             :: Type n }

        -- | Witness provided for a forget does not witness emptiness.
        | ErrorWitnessNotEmpty
        { errorChecking         :: Exp a n
        , errorWitness          :: Witness n
        , errorType             :: Type n }


        -- Case Expressions -------------------------------
        -- | Discriminant of case expression is not algebraic data.
        | ErrorCaseDiscrimNotAlgebraic
        { errorChecking         :: Exp a n
        , errorTypeDiscrim      :: Type n }

        -- | We don't have a data type declaration for the type of the discriminant.
        | ErrorCaseDiscrimTypeUndeclared
        { errorChecking         :: Exp a n 
        , errorTypeDiscrim      :: Type n }

        -- | Case expression has no alternatives.
        | ErrorCaseNoAlternatives
        { errorChecking         :: Exp a n }

        -- | Case alternatives doesn't match all constructors.
        | ErrorCaseNonExhaustive
        { errorChecking         :: Exp a n
        , errorCtorNamesMissing :: [n] }

        -- | Case alternatives doesn't match all constructors.
        --   For large types where there are too many missing constructors to list.
        | ErrorCaseNonExhaustiveLarge
        { errorChecking         :: Exp a n }

        -- | Case alternatives are overlapping.
        | ErrorCaseOverlapping
        { errorChecking         :: Exp a n }

        -- | Too many binders in alternative.
        | ErrorCaseTooManyBinders
        { errorChecking         :: Exp a n
        , errorCtorBound        :: Bound n
        , errorCtorFields       :: Int
        , errorPatternFields    :: Int }

        -- | Cannot instantiate constructor type with type args of discriminant.
        | ErrorCaseCannotInstantiate
        { errorChecking         :: Exp a n
        , errorTypeCtor         :: Type n
        , errorTypeDiscrim      :: Type n }

        -- | Type of discriminant does not match type of pattern.
        | ErrorCaseDiscrimTypeMismatch
        { errorChecking         :: Exp a n
        , errorTypeDiscrim      :: Type n
        , errorTypePattern      :: Type n }

        -- | Annotation on pattern variable does not match field type of constructor.
        | ErrorCaseFieldTypeMismatch
        { errorChecking         :: Exp a n
        , errorTypeAnnot        :: Type n
        , errorTypeField        :: Type n }

        -- | Result types of case expression are not identical.
        | ErrorCaseAltResultMismatch
        { errorChecking         :: Exp a n
        , errorAltType1         :: Type n
        , errorAltType2         :: Type n }


instance (Pretty n, Eq n) => Pretty (Error a n) where
 ppr err
  = case err of
        ErrorType err'  -> ppr err'

        ErrorMalformedExp xx
         -> vcat [ text "Malformed expression: "        <> ppr xx ]
        
        ErrorMalformedType xx tt
         -> vcat [ text "Found malformed type: "        <> ppr tt
                 , text "       when checking: "        <> ppr xx ]

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
                 , text "       in application: "       <> ppr xx ]
         
        ErrorAppNotFun xx t1 t2
         -> vcat [ text "Cannot apply non-function"
                 , text "              of type: "       <> ppr t1
                 , text "  to argument of type: "       <> ppr t2 
                 , text "       in application: "       <> ppr xx ]


        -- Lambda -----------------------------------------
        ErrorLamNotPure xx eff
         -> vcat [ text "Impure type abstraction"
                 , text "           has effect: "       <> ppr eff
                 , text "        when checking: "       <> ppr xx ]
                 
        
        ErrorLamBindNotData xx t1 k1
         -> vcat [ text "Function parameter does not have data kind."
                 , text "    The function parameter:"   <> ppr t1
                 , text "                  has kind: "  <> ppr k1
                 , text "            but it must be: *"
                 , text "             when checking: "  <> ppr xx ]

        ErrorLamBodyNotData xx b1 t2 k2
         -> vcat [ text "Result of function does not have data kind."
                 , text "   In function with binder: "  <> ppr b1
                 , text "       the result has type: "  <> ppr t2
                 , text "                 with kind: "  <> ppr k2
                 , text "            but it must be: *"
                 , text "             when checking: "  <> ppr xx ]

        ErrorLamReboundSpec xx b1
         -> vcat [ text "Cannot shadow level-1 binder: " <> ppr b1
                 , text "  when checking: " <> ppr xx ]
        

        -- Let --------------------------------------------
        ErrorLetMismatch xx b t
         -> vcat [ text "Type mismatch in let-binding."
                 , text "                The binder: "  <> ppr (binderOfBind b)
                 , text "                  has type: "  <> ppr (typeOfBind b)
                 , text "     but the body has type: "  <> ppr t
                 , text "             when checking: "  <> ppr xx ]

        ErrorLetBindingNotData xx b k
         -> vcat [ text "Let binding does not have data kind."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "             has type: "       <> ppr (typeOfBind b)
                 , text "            with kind: "       <> ppr k
                 , text "       but it must be: * "
                 , text "        when checking: "       <> ppr xx ]

        ErrorLetBodyNotData xx t k
         -> vcat [ text "Let body does not have data kind."
                 , text " Body of let has type: "       <> ppr t
                 , text "            with kind: "       <> ppr k
                 , text "       but it must be: * "
                 , text "        when checking: "       <> ppr xx ]


        -- Let Lazy ---------------------------------------
        ErrorLetLazyNotEmpty xx b clo
         -> vcat [ text "Lazy let binding is not empty."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "          has closure: "       <> ppr clo
                 , text "        when checking: "       <> ppr xx ]

        ErrorLetLazyNotPure xx b eff
         -> vcat [ text "Lazy let binding is not pure."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "           has effect: "       <> ppr eff
                 , text "        when checking: "       <> ppr xx ]

        ErrorLetLazyNoWitness xx b t
         -> vcat [ text "Lazy let binding has no witness but the bound value may have a head region."
                 , text "      The binding for: "       <> ppr (binderOfBind b)
                 , text "             Has type: "       <> ppr t
                 , text "        when checking: "       <> ppr xx ]

        ErrorLetLazyWitnessTypeMismatch xx b tWitGot tBind tWitExp
         -> vcat [ text "Unexpected witness type in lazy let binding."
                 , text "          The binding for: "   <> ppr (binderOfBind b)
                 , text "    has a witness of type: "   <> ppr tWitGot
                 , text "           but is type is: "   <> ppr tBind
                 , text " so the witness should be: "   <> ppr tWitExp 
                 , text "            when checking: "   <> ppr xx ]

        -- Letrec -----------------------------------------
        ErrorLetrecBindingNotLambda xx x
         -> vcat [ text "Letrec can only bind lambda abstractions."
                 , text "      This is not one: "       <> ppr x
                 , text "        when checking: "       <> ppr xx ]


        -- Letregion --------------------------------------
        ErrorLetRegionNotRegion xx b k
         -> vcat [ text "Letregion binder does not have region kind."
                 , text "        Region binder: "       <> ppr b
                 , text "             has kind: "       <> ppr k
                 , text "       but is must be: %" 
                 , text "        when checking: "       <> ppr xx ]

        ErrorLetRegionRebound xx b
         -> vcat [ text "Region variable shadows existing one."
                 , text "           Region variable: "  <> ppr b
                 , text "     is already in environment"
                 , text "             when checking: "  <> ppr xx]

        ErrorLetRegionFree xx b t
         -> vcat [ text "Region variable escapes scope of letregion."
                 , text "       The region variable: "  <> ppr b
                 , text "  is free in the body type: "  <> ppr t
                 , text "             when checking: "  <> ppr xx ]
        
        ErrorLetRegionWitnessInvalid xx b
         -> vcat [ text "Invalid witness type with letregion."
                 , text "          The witness: "       <> ppr b
                 , text "  cannot be created with a letregion"
                 , text "        when checking: "       <> ppr xx]

        ErrorLetRegionWitnessConflict xx b1 b2
         -> vcat [ text "Conflicting witness types with letregion."
                 , text "      Witness binding: "       <> ppr b1
                 , text "       conflicts with: "       <> ppr b2 
                 , text "        when checking: "       <> ppr xx]

        ErrorLetRegionWitnessOther xx b1 b2
         -> vcat [ text "Witness type is not for bound region."
                 , text "      letregion binds: "       <> ppr b1
                 , text "  but witness type is: "       <> ppr b2
                 , text "        when checking: "       <> ppr xx]

        ErrorWithRegionNotRegion xx u k
         -> vcat [ text "Withregion handle does not have region kind."
                 , text "   Region var or ctor: "       <> ppr u
                 , text "             has kind: "       <> ppr k
                 , text "       but it must be: %"
                 , text "        when checking: "       <> ppr xx]

        -- Witnesses --------------------------------------
        ErrorWAppMismatch ww t1 t2
         -> vcat [ text "Type mismatch in witness application."
                 , text "  Constructor expects: "       <> ppr t1
                 , text "      but argument is: "       <> ppr t2
                 , text "        when checking: "       <> ppr ww]

        ErrorWAppNotCtor ww t1 t2
         -> vcat [ text "Type cannot apply non-constructor witness"
                 , text "              of type: "       <> ppr t1
                 , text "  to argument of type: "       <> ppr t2 
                 , text "        when checking: "       <> ppr ww ]

        ErrorCannotJoin ww w1 t1 w2 t2
         -> vcat [ text "Cannot join witnesses."
                 , text "          Cannot join: "       <> ppr w1
                 , text "              of type: "       <> ppr t1
                 , text "         with witness: "       <> ppr w2
                 , text "              of type: "       <> ppr t2
                 , text "        when checking: "       <> ppr ww ]

        ErrorWitnessNotPurity xx w t
         -> vcat [ text "Witness for a purify does not witness purity."
                 , text "        Witness: "             <> ppr w
                 , text "       has type: "             <> ppr t
                 , text "  when checking: "             <> ppr xx ]

        ErrorWitnessNotEmpty xx w t
         -> vcat [ text "Witness for a forget does not witness emptiness."
                 , text "        Witness: "             <> ppr w
                 , text "       has type: "             <> ppr t
                 , text "  when checking: "             <> ppr xx ]


        -- Case Expressions -------------------------------
        ErrorCaseDiscrimNotAlgebraic xx tDiscrim
         -> vcat [ text "Discriminant of case expression is not algebraic data."
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , text "         when checking: "      <> ppr xx ]
        
        ErrorCaseDiscrimTypeUndeclared xx tDiscrim
         -> vcat [ text "Type of discriminant does not have a data declaration."
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , text "         when checking: "      <> ppr xx ]

        ErrorCaseNoAlternatives xx
         -> vcat [ text "Case expression does not have any alternatives."
                 , text "         when checking: "      <> ppr xx ]

        ErrorCaseNonExhaustive xx ns
         -> vcat [ text "Case alternatives are non-exhaustive."
                 , text " Constructors not matched: "   
                        <> (sep $ punctuate comma $ map ppr ns)
                 , text "            when checking: "   <> ppr xx ]

        ErrorCaseNonExhaustiveLarge xx
         -> vcat [ text "Case alternatives are non-exhaustive."
                 , text "  when checking: "             <> ppr xx ]

        ErrorCaseOverlapping xx
         -> vcat [ text "Case alternatives are overlapping."
                 , text "  when checking: "             <> ppr xx ]

        ErrorCaseTooManyBinders xx uCtor iCtorFields iPatternFields
         -> vcat [ text "Pattern has more binders than there are fields in the constructor."
                 , text "     Contructor: " <> ppr uCtor
                 , text "            has: " <> ppr iCtorFields      
                                            <+> text "fields"
                 , text "  but there are: " <> ppr iPatternFields   
                                           <+> text "binders in the pattern" 
                 , text "  when checking: " <> ppr xx ]

        ErrorCaseCannotInstantiate xx tCtor tDiscrim
         -> vcat [ text "Cannot instantiate constructor type with discriminant type args."
                 , text " Either the constructor has an invalid type,"
                 , text " or the type of the discriminant does not match the type of the pattern."
                 , text "      Constructor type: "      <> ppr tCtor
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , text "         when checking: "      <> ppr xx ]

        ErrorCaseDiscrimTypeMismatch xx tDiscrim tPattern
         -> vcat [ text "Discriminant type does not match result of pattern type."
                 , text "     Discriminant type: "      <> ppr tDiscrim
                 , text "          Pattern type: "      <> ppr tPattern
                 , text "         when checking: "      <> ppr xx ]

        ErrorCaseFieldTypeMismatch xx tAnnot tField
         -> vcat [ text "Annotation on pattern variable does not match type of field."
                 , text "       Annotation type: "      <> ppr tAnnot
                 , text "            Field type: "      <> ppr tField
                 , text "         when checking: "      <> ppr xx ]

        ErrorCaseAltResultMismatch xx t1 t2
         -> vcat [ text "Mismatch in alternative result types."
                 , text "   Type of alternative: "      <> ppr t1
                 , text "        does not match: "      <> ppr t2
                 , text "         when checking: "      <> ppr xx ]

