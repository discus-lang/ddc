-- | Errors produced when checking core expressions.
module DDC.Core.Check.Error
        (Error(..))
where
import DDC.Core.Exp
import DDC.Type.Universe
import qualified DDC.Type.Check as T


-- | All the things that can go wrong when type checking an expression
--   or witness.
data Error a n
        -- Type -------------------------------------------
        -- | Found a kind error when checking a type.
        = ErrorType
        { errorTypeError        :: T.Error n }

        -- | Found a malformed type,
        --   and we don't have a more specific diagnosis.
        | ErrorMalformedType
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorType             :: Type n }


        -- Module -----------------------------------------
        -- | Exported value is undefined.
        | ErrorExportUndefined
        { errorName             :: n }

        -- | Type signature of exported binding does not match the type at
        --   the definition site.
        | ErrorExportMismatch
        { errorName             :: n
        , errorExportType       :: Type n
        , errorDefType          :: Type n }


        -- Exp --------------------------------------------
        -- | Found a malformed expression, 
        --   and we don't have a more specific diagnosis.
        | ErrorMalformedExp
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }


        -- Var --------------------------------------------
        -- | An undefined type variable.
        | ErrorUndefinedVar
        { errorAnnot            :: a
        , errorBound            :: Bound n 
        , errorUniverse         :: Universe }

        -- | A bound occurrence of a variable whose type annotation does not match
        --   the corresponding annotation in the environment.
        | ErrorVarAnnotMismatch
        { errorAnnot            :: a
        , errorBound            :: Bound n
        , errorTypeAnnot        :: Type n
        , errorTypeEnv          :: Type n }


        -- Con --------------------------------------------
        -- | A data constructor that wasn't in the set of data definitions.
        | ErrorUndefinedCtor
        { errorAnnot            :: a
        , errorChecking         :: Exp a n }


        -- Application ------------------------------------
        -- | A function application where the parameter and argument don't match.
        | ErrorAppMismatch
        { errrorAnnot           :: a
        , errorChecking         :: Exp a n
        , errorParamType        :: Type n
        , errorArgType          :: Type n }

        -- | Tried to apply something that is not a function.
        | ErrorAppNotFun
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorNotFunType       :: Type n
        , errorArgType          :: Type n }


        -- Lambda -----------------------------------------
        -- | A type abstraction that tries to shadow a type variable that is
        --   already in the environment.
        | ErrorLamShadow
        { errorAnnot            :: a
        , errorChecking         :: Exp a n 
        , errorBind             :: Bind n }

        -- | An abstraction where the body has a visible side effect that 
        --   is not supported by the current language fragment.
        | ErrorLamNotPure
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorUniverse         :: Universe
        , errorEffect           :: Effect n }

        -- | An abstraction where the body has a visible closure that 
        --   is not supported by the current language fragment.
        | ErrorLamNotEmpty
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorUniverse         :: Universe
        , errorClosure          :: Closure n }

        -- | A value function where the parameter does not have data kind.
        | ErrorLamBindNotData
        { errorAnnot            :: a
        , errorChecking         :: Exp a n 
        , errorType             :: Type n
        , errorKind             :: Kind n }

        -- | An abstraction where the body does not have data kind.
        | ErrorLamBodyNotData
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n
        , errorKind             :: Kind n }


        -- Let --------------------------------------------
        -- | A let-expression where the type of the binder does not match the right
        --   of the binding.
        | ErrorLetMismatch
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }

        -- | A let-expression where the right of the binding does not have data kind.
        | ErrorLetBindingNotData
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorKind             :: Kind n }

        -- | A let-expression where the body does not have data kind.
        | ErrorLetBodyNotData
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

        -- | A recursive let-expression that has more than one binding
        --   with the same name.
        | ErrorLetrecRebound
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n }


        -- Letregion --------------------------------------
        -- | A letregion-expression where the some of the bound variables do not
        --   have region kind.
        | ErrorLetRegionsNotRegion
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBinds            :: [Bind n]
        , errorKinds            :: [Kind n] }

        -- | A letregion-expression that tried to shadow some pre-existing named
        --   region variables.
        | ErrorLetRegionsRebound
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBinds            :: [Bind n] }

        -- | A letregion-expression where some of the the bound region variables
        --   are free in the type of the body.
        | ErrorLetRegionFree
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBinds            :: [Bind n]
        , errorType             :: Type n }

        -- | A letregion-expression that tried to create a witness with an 
        --   invalid type.
        | ErrorLetRegionWitnessInvalid
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBind             :: Bind n }

        -- | A letregion-expression that tried to create conflicting witnesses.
        | ErrorLetRegionWitnessConflict
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBindWitness1     :: Bind n
        , errorBindWitness2     :: Bind n }

        -- | A letregion-expression where a bound witnesses was not for the
        --   the region variable being introduced.
        | ErrorLetRegionsWitnessOther
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBoundRegions     :: [Bound n]
        , errorBindWitness      :: Bind  n }

        -- | A letregion-expression where the witness binding references some
        --   free region variable that is not the one being introduced.
        | ErrorLetRegionWitnessFree
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBindWitness      :: Bind n }
        

        -- Withregion -------------------------------------
        -- | A withregion-expression where the handle does not have region kind.
        | ErrorWithRegionNotRegion
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBound            :: Bound n
        , errorKind             :: Kind n }

        -- | A letregion-expression where some of the the bound region variables
        --   are free in the type of the body.
        | ErrorWithRegionFree
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorBound            :: Bound n
        , errorType             :: Type n }


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

        -- | An invalid witness join.
        | ErrorCannotJoin
        { errorAnnot            :: a
        , errorWitness          :: Witness a n
        , errorWitnessLeft      :: Witness a n
        , errorTypeLeft         :: Type n
        , errorWitnessRight     :: Witness a n
        , errorTypeRight        :: Type n }

        -- | A witness provided for a purify cast that does not witness purity.
        | ErrorWitnessNotPurity
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorWitness          :: Witness a n
        , errorType             :: Type n }

        -- | A witness provided for a forget cast that does not witness emptiness.
        | ErrorWitnessNotEmpty
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
        , errorCtorDaCon        :: DaCon n
        , errorCtorFields       :: Int
        , errorPatternFields    :: Int }

        -- | A case-expression where the pattern types could not be instantiated
        --   with the arguments of the scrutinee type.
        | ErrorCaseCannotInstantiate
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n 
        , errorTypeCtor         :: Type n }

        -- | A case-expression where the type of the scrutinee does not match
        --   the type of the pattern.
        | ErrorCaseScrutineeTypeMismatch
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n
        , errorTypePattern      :: Type n }

        -- | A case-expression where the annotation on a pattern variable binder
        --   does not match the field type of the constructor.
        | ErrorCaseFieldTypeMismatch
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorTypeAnnot        :: Type n
        , errorTypeField        :: Type n }

        -- | A case-expression where the result types of the alternatives are not
        --   identical.
        | ErrorCaseAltResultMismatch
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorAltType1         :: Type n
        , errorAltType2         :: Type n }


        -- Casts ------------------------------------------
        -- | A weakeff-cast where the type provided does not have effect kind.
        | ErrorWeakEffNotEff
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorEffect           :: Effect n
        , errorKind             :: Kind n }

        -- | A run cast applied to a non-suspension.
        | ErrorRunNotSuspension
        { errorAnnot            :: a
        , errorChecking         :: Exp a n
        , errorType             :: Type n }


        -- Types ------------------------------------------
        -- | Found a naked `XType` that wasn't the argument of an application.
        | ErrorNakedType
        { errorChecking         :: Exp a n }


        -- Witnesses --------------------------------------
        -- | Found a naked `XWitness` that wasn't the argument of an application.
        | ErrorNakedWitness
        { errorChecking         :: Exp a n }
        deriving (Show)

