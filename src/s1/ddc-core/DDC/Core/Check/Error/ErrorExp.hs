{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Error.ErrorExp
        (Error(..))
where
import DDC.Core.Check.Error.ErrorType
import DDC.Core.Check.Error.ErrorData
import DDC.Core.Exp
import DDC.Type.Universe

-- | All the things that can go wrong when type checking an expression
--   or witness.
data Error a n
        -- Type -------------------------------------------
        -- | Found a kind error when checking a type.
        = ErrorType
        { errorTypeError        :: ErrorType n }

        -- | Found an error in the data type definitions.
        | ErrorData
        { errorData             :: ErrorData n }

        -- Module -----------------------------------------
        -- | Exported value is undefined.
        | ErrorExportUndefined
        { errorName             :: n }

        -- | Exported name is exported multiple times.
        | ErrorExportDuplicate
        { errorName             :: n }

        -- | Type signature of exported binding does not match the type at
        --   the definition site.
        | ErrorExportMismatch
        { errorName             :: n
        , errorExportType       :: Type n
        , errorDefType          :: Type n }

        -- | Imported name is imported multiple times.
        | ErrorImportDuplicate
        { errorName             :: n }

        -- | An imported capability that does not have kind Effect.
        | ErrorImportCapNotEffect
        { errorName             :: n }

        -- | An imported value that doesn't have kind Data.
        | ErrorImportValueNotData
        { errorName             :: n }


        -- Exp --------------------------------------------
        -- | Generic mismatch between expected and inferred types.
        | ErrorMismatch
        { errorAnnot            :: a
        , errorInferred         :: Type n
        , errorExpected         :: Type n
        , errorChecking         :: Exp a n }


        -- Var --------------------------------------------
        -- | An undefined type variable.
        | ErrorUndefinedVar
        { errorAnnot            :: a
        , errorBound            :: Bound n
        , errorUniverse         :: Universe }


        -- Con --------------------------------------------
        -- | A data constructor that wasn't in the set of data definitions.
        | ErrorUndefinedCtor
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }


        -- Application ------------------------------------
        -- | Tried to apply something that is not a function.
        | ErrorAppNotFun
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorNotFunType       :: Type n }

        -- | Cannot infer type of polymorphic expression.
        | ErrorAppCannotInferPolymorphic
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }


        -- Abs -------------------------------------------
        -- | A type abstraction that tries to shadow a type variable that is
        --   already in the environment.
        | ErrorAbsShadow
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n }

        -- | A function abstraction without a type annotation on the parameter.
        --   This is only an error when checking in Recon mode.
        | ErrorAbsParamUnannotated
        { errorAnnot            :: a
        , errorBind             :: Bind n }

        -- | An abstraction where the body has a visible side effect that
        --   is not supported by the current language fragment.
        | ErrorAbsNotPure
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorUniverse         :: Universe
        , errorEffect           :: Effect n }

        -- | An abstraction where the parameter type does not have a valid kind.
        | ErrorAbsBindBadKind
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorType             :: Type n
        , errorKind             :: Kind n }


        -- Letrec -----------------------------------------
        -- | A recursive let-expression where the right of the binding is not
        --   a lambda abstraction.
        | ErrorLetrecBindingNotLambda
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorExp              :: Exp a n }

        -- | A recursive let-binding with a missing type annotation.
        | ErrorLetrecMissingAnnot
        { errorAnnot            :: a
        , errorBind             :: Bind n
        , errorExp              :: Exp a n }

        -- | A recursive let-expression that has more than one binding
        --   with the same name.
        | ErrorLetrecRebound
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n }


        -- Private --------------------------------------
        -- | A letregion-expression where the some of the bound variables do not
        --   have region kind.
        --   This is an internal error. The concrete syntax of the language should
        --   not allow us to write region binders on private constructs that
        --   do not have region kind.
        | ErrorPrivateNotRegion
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBinds            :: [Bind n]
        , errorKinds            :: [Kind n] }

        -- | A letregion-expression that tried to shadow some pre-existing named
        --   region variables.
        | ErrorPrivateRebound
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBinds            :: [Bind n] }

        -- | A letregion-expression where some of the the bound region variables
        --   are free in the type of the body.
        | ErrorPrivateEscape
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBinds            :: [Bind n]
        , errorType             :: Type n }

        -- | A letregion-expression that tried to create a witness with an
        --   invalid type.
        | ErrorPrivateWitnessInvalid
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n }

        -- | A letregion-expression that tried to create conflicting witnesses.
        | ErrorPrivateWitnessConflict
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBindWitness1     :: Bind n
        , errorBindWitness2     :: Bind n }

        -- | A letregion-expression where a bound witnesses was not for the
        --   the region variable being introduced.
        | ErrorPrivateWitnessOther
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBoundRegions     :: [Bound n]
        , errorBindWitness      :: Bind  n }


        -- Witnesses --------------------------------------
        -- | A witness application where the argument type does not match
        --   the parameter type.
        | ErrorWAppMismatch
        { errorAnnot            :: a
        , errorWitness          :: Witness a n
        , errorParamType        :: Type n
        , errorArgType          :: Type n }

        -- | Tried to perform a witness application with a non-witness.
        | ErrorWAppNotCtor
        { errorAnnot            :: a
        , errorWitness          :: Witness a n
        , errorNotFunType       :: Type n
        , errorArgType          :: Type n }

        -- | A witness provided for a purify cast that does not witness purity.
        | ErrorWitnessNotPurity
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorWitness          :: Witness a n
        , errorType             :: Type n }


        -- Case Expressions -------------------------------
        -- | A case-expression where the scrutinee type is not algebraic.
        | ErrorCaseScrutineeNotAlgebraic
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n }

        -- | A case-expression where the scrutinee type is not in our set
        --   of data type declarations.
        --
        --   This is an internal error. There shouldn't be any way to
        --   introduce a value of an undeclared type.
        --
        | ErrorCaseScrutineeTypeUndeclared
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n }

        -- | A case-expression with no alternatives.
        | ErrorCaseNoAlternatives
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }

        -- | A case-expression where the alternatives don't cover all the
        --   possible data constructors.
        | ErrorCaseNonExhaustive
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorCtorNamesMissing :: [n] }

        -- | A case-expression where the alternatives don't cover all the
        --   possible constructors, and the type has too many data constructors
        --   to list.
        | ErrorCaseNonExhaustiveLarge
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }

        -- | A case-expression with overlapping alternatives.
        | ErrorCaseOverlapping
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }

        -- | A case-expression where one of the patterns has too many binders.
        | ErrorCaseTooManyBinders
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorCtorDaCon        :: DaCon n (Type n)
        , errorCtorFields       :: Int
        , errorPatternFields    :: Int }

        -- | A case-expression where the pattern types could not be instantiated
        --   with the arguments of the scrutinee type.
        --
        --   This is an internal error. The structure of data type declarations
        --   should be checked so that the result type of each data constructor
        --   matches the data type being declared.
        --
        | ErrorCaseCannotInstantiate
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n
        , errorTypeCtor         :: Type n }

        -- | A case-expression where the annotation on a pattern variable binder
        --   does not match the field type of the constructor.
        | ErrorCaseFieldTypeMismatch
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorTypeAnnot        :: Type n
        , errorTypeField        :: Type n }


        -- Casts ------------------------------------------
        -- | A run cast applied to a non-suspension.
        | ErrorRunNotSuspension
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorType             :: Type n }

        -- | A run cast where the context does not support the suspended effect.
        | ErrorRunNotSupported
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorEffect           :: Effect n }

        -- | A run cast where we cannot infer the type of the suspended computation
        --   and thus cannot check if its effects are suppored by the context.
        | ErrorRunCannotInfer
        { errorAnnot            :: a
        , errorExp              :: Exp a n }


        -- Witnesses --------------------------------------
        -- | Found a naked `XWitness` that wasn't the argument of an application.
        | ErrorNakedWitness
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }

        -- Post check errors ------------------------------
        | ErrorAmbiguousType
        { errorAnnot            :: a }

        | ErrorAmbiguousTypeExp
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }
        deriving (Show)

