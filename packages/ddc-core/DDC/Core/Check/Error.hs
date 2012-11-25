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
        { errorChecking         :: Exp a n
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
        { errorChecking         :: Exp a n }


        -- Var --------------------------------------------
        -- | An undefined type variable.
        | ErrorUndefinedVar
        { errorBound            :: Bound n 
        , errorUniverse         :: Universe }

        -- | A bound occurrence of a variable whose type annotation does not match
        --   the corresponding annotation in the environment.
        | ErrorVarAnnotMismatch
        { errorBound            :: Bound n
        , errorTypeAnnot        :: Type n
        , errorTypeEnv          :: Type n }


        -- Con --------------------------------------------
        -- | A data constructor that wasn't in the set of data definitions.
        | ErrorUndefinedCtor
        { errorChecking         :: Exp a n }


        -- Application ------------------------------------
        -- | A function application where the parameter and argument don't match.
        | ErrorAppMismatch
        { errorChecking         :: Exp a n
        , errorParamType        :: Type n
        , errorArgType          :: Type n }

        -- | Tried to apply something that is not a function.
        | ErrorAppNotFun
        { errorChecking         :: Exp a n
        , errorNotFunType       :: Type n
        , errorArgType          :: Type n }


        -- Lambda -----------------------------------------
        -- | A type abstraction that tries to shadow a type variable that is
        --   already in the environment.
        | ErrorLamShadow
        { errorChecking         :: Exp a n 
        , errorBind             :: Bind n }

        -- | A type or witness abstraction where the body has a visible side effect.
        | ErrorLamNotPure
        { errorChecking         :: Exp a n
        , errorSpecOrWit        :: Bool
        , errorEffect           :: Effect n }

        -- | A value function where the parameter does not have data kind.
        | ErrorLamBindNotData
        { errorChecking         :: Exp a n 
        , errorType             :: Type n
        , errorKind             :: Kind n }

        -- | An abstraction where the body does not have data kind.
        | ErrorLamBodyNotData
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n
        , errorKind             :: Kind n }


        -- Let --------------------------------------------
        -- | A let-expression where the type of the binder does not match the right
        --   of the binding.
        | ErrorLetMismatch
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }

        -- | A let-expression where the right of the binding does not have data kind.
        | ErrorLetBindingNotData
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorKind             :: Kind n }

        -- | A let-expression where the body does not have data kind.
        | ErrorLetBodyNotData
        { errorChecking         :: Exp a n
        , errorType             :: Type n
        , errorKind             :: Kind n }


        -- Let Lazy ---------------------------------------
        -- | A lazy let binding that has a visible side effect.
        | ErrorLetLazyNotPure
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorEffect           :: Effect n }

        -- | A lazy let binding with a non-empty closure.
        | ErrorLetLazyNotEmpty
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorClosure          :: Closure n }

        -- | A lazy let binding without a witness that binding is in a lazy region.
        | ErrorLetLazyNoWitness
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }

        -- | A lazy let binding where the witness has the wrong type.
        | ErrorLetLazyWitnessTypeMismatch 
        { errorChecking          :: Exp a n
        , errorBind              :: Bind n
        , errorWitnessTypeHave   :: Type n
        , errorBindType          :: Type n
        , errorWitnessTypeExpect :: Type n }


        -- Letrec -----------------------------------------
        -- | A recursive let-expression where the right of the binding is not
        --   a lambda abstraction.
        | ErrorLetrecBindingNotLambda
        { errorChecking         :: Exp a n 
        , errorExp              :: Exp a n }

        -- | A recursive let-expression that has more than one binding
        --   with the same name.
        | ErrorLetrecRebound
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n }


        -- Letregion --------------------------------------
        -- | A letregion-expression where the some of the bound variables do not
        --   have region kind.
        | ErrorLetRegionsNotRegion
        { errorChecking         :: Exp a n
        , errorBinds            :: [Bind n]
        , errorKinds            :: [Kind n] }

        -- | A letregion-expression that tried to shadow some pre-existing named
        --   region variables.
        | ErrorLetRegionsRebound
        { errorChecking         :: Exp a n
        , errorBinds            :: [Bind n] }

        -- | A letregion-expression where some of the the bound region variables
        --   are free in the type of the body.
        | ErrorLetRegionFree
        { errorChecking         :: Exp a n
        , errorBinds            :: [Bind n]
        , errorType             :: Type n }

        -- | A letregion-expression that tried to create a witness with an 
        --   invalid type.
        | ErrorLetRegionWitnessInvalid
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n }

        -- | A letregion-expression that tried to create conflicting witnesses.
        | ErrorLetRegionWitnessConflict
        { errorChecking         :: Exp a n
        , errorBindWitness1     :: Bind n
        , errorBindWitness2     :: Bind n }

        -- | A letregion-expression where a bound witnesses was not for the
        --   the region variable being introduced.
        | ErrorLetRegionsWitnessOther
        { errorChecking         :: Exp a n
        , errorBoundRegions     :: [Bound n]
        , errorBindWitness      :: Bind  n }

        -- | A letregion-expression where the witness binding references some
        --   free region variable that is not the one being introduced.
        | ErrorLetRegionWitnessFree
        { errorChecking         :: Exp a n
        , errorBindWitness      :: Bind n }
        
        -- | A withregion-expression where the handle does not have region kind.
        | ErrorWithRegionNotRegion
        { errorChecking         :: Exp a n
        , errorBound            :: Bound n
        , errorKind             :: Kind n }


        -- Witnesses --------------------------------------
        -- | A witness application where the argument type does not match
        --   the parameter type.
        | ErrorWAppMismatch
        { errorWitness          :: Witness n
        , errorParamType        :: Type n
        , errorArgType          :: Type n }

        -- | Tried to perform a witness application with a non-witness.
        | ErrorWAppNotCtor
        { errorWitness          :: Witness n
        , errorNotFunType       :: Type n
        , errorArgType          :: Type n }

        -- | An invalid witness join.
        | ErrorCannotJoin
        { errorWitness          :: Witness n
        , errorWitnessLeft      :: Witness n
        , errorTypeLeft         :: Type n
        , errorWitnessRight     :: Witness n
        , errorTypeRight        :: Type n }

        -- | A witness provided for a purify cast that does not witness purity.
        | ErrorWitnessNotPurity
        { errorChecking         :: Exp a n
        , errorWitness          :: Witness n
        , errorType             :: Type n }

        -- | A witness provided for a forget cast that does not witness emptiness.
        | ErrorWitnessNotEmpty
        { errorChecking         :: Exp a n
        , errorWitness          :: Witness n
        , errorType             :: Type n }


        -- Case Expressions -------------------------------
        -- | A case-expression where the scrutinee type is not algebraic.
        | ErrorCaseScrutineeNotAlgebraic
        { errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n }

        -- | A case-expression where the scrutinee type is not in our set
        --   of data type declarations.
        | ErrorCaseScrutineeTypeUndeclared
        { errorChecking         :: Exp a n 
        , errorTypeScrutinee    :: Type n }

        -- | A case-expression with no alternatives.
        | ErrorCaseNoAlternatives
        { errorChecking         :: Exp a n }

        -- | A case-expression where the alternatives don't cover all the
        --   possible data constructors.
        | ErrorCaseNonExhaustive
        { errorChecking         :: Exp a n
        , errorCtorNamesMissing :: [n] }

        -- | A case-expression where the alternatives don't cover all the
        --   possible constructors, and the type has too many data constructors
        --   to list.
        | ErrorCaseNonExhaustiveLarge
        { errorChecking         :: Exp a n }

        -- | A case-expression with overlapping alternatives.
        | ErrorCaseOverlapping
        { errorChecking         :: Exp a n }

        -- | A case-expression where one of the patterns has too many binders.
        | ErrorCaseTooManyBinders
        { errorChecking         :: Exp a n
        , errorCtorDaCon        :: DaCon n
        , errorCtorFields       :: Int
        , errorPatternFields    :: Int }

        -- | A case-expression where the pattern types could not be instantiated
        --   with the arguments of the scrutinee type.
        | ErrorCaseCannotInstantiate
        { errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n 
        , errorTypeCtor         :: Type n }

        -- | A case-expression where the type of the scrutinee does not match
        --   the type of the pattern.
        | ErrorCaseScrutineeTypeMismatch
        { errorChecking         :: Exp a n
        , errorTypeScrutinee    :: Type n
        , errorTypePattern      :: Type n }

        -- | A case-expression where the annotation on a pattern variable binder
        --   does not match the field type of the constructor.
        | ErrorCaseFieldTypeMismatch
        { errorChecking         :: Exp a n
        , errorTypeAnnot        :: Type n
        , errorTypeField        :: Type n }

        -- | A case-expression where the result types of the alternatives are not
        --   identical.
        | ErrorCaseAltResultMismatch
        { errorChecking         :: Exp a n
        , errorAltType1         :: Type n
        , errorAltType2         :: Type n }


        -- Casts ------------------------------------------
        -- | A weakeff-cast where the type provided does not have effect kind.
        | ErrorWeakEffNotEff
        { errorChecking         :: Exp a n
        , errorEffect           :: Effect n
        , errorKind             :: Kind n }


        -- Types ------------------------------------------
        -- | Found a naked `XType` that wasn't the argument of an application.
        | ErrorNakedType
        { errorChecking         :: Exp a n }


        -- Witnesses --------------------------------------
        -- | Found a naked `XWitness` that wasn't the argument of an application.
        | ErrorNakedWitness
        { errorChecking         :: Exp a n }
        deriving (Show)

