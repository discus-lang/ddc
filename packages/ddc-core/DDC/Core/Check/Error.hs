-- | Errors produced when checking core expressions.
module DDC.Core.Check.Error
        (Error(..))
where
import DDC.Core.Exp
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

        -- | Found a naked type that wasn't the argument of an application.
        | ErrorNakedType
        { errorChecking         :: Exp a n }

        -- | Found a naked witness that wasn't the argument of an application.
        | ErrorNakedWitness
        { errorChecking         :: Exp a n }

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


        -- Casts ------------------------------------------
        -- | Type provided to a 'maxeff' does not have effect kind.
        | ErrorMaxeffNotEff
        { errorChecking         :: Exp a n
        , errorEffect           :: Effect n
        , errorKind             :: Kind n }

        -- | Type provided to a 'maxclo' does not have effect kind.
        | ErrorMaxcloNotClo
        { errorChecking         :: Exp a n
        , errorClosure          :: Closure n
        , errorKind             :: Kind n }

        -- | Closure provided to a 'maxclo' is malformed.
        --   It can only contain 'Use' terms.
        | ErrorMaxcloMalformed
        { errorChecking         :: Exp a n 
        , errorClosure          :: Closure n }
        deriving (Show)

