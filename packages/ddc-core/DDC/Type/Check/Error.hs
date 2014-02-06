
-- | Errors produced when checking types.
module DDC.Type.Check.Error
        ( Error         (..)
        , ErrorData     (..))
where
import DDC.Type.Universe
import DDC.Type.Exp


-- | Things that can go wrong when checking the kind of at type.
data Error n
        -- Generic Problems ---------------------
        -- | Tried to check a type using the wrong universe, 
        --   for example: asking for the kind of a kind.
        = ErrorUniverseMalfunction
        { errorType             :: Type n
        , errorUniverse         :: Universe }

        -- | Generic kind mismatch.
        | ErrorMismatch
        { errorUniverse         :: Universe
        , errorInferred         :: Type n
        , errorExpected         :: Type n
        , errorChecking         :: Type n }

        -- | Cannot infer a type.
        | ErrorCannotInfer
        { errorChecking         :: Type n }


        -- Variables ----------------------------
        -- | An undefined type variable.
        | ErrorUndefined        
        { errorBound            :: Bound n }

        -- | The kind annotation on the variables does not match the one in the
        --   environment.
        | ErrorVarAnnotMismatch
        { errorBound            :: Bound n
        , errorTypeEnv          :: Type n }


        -- Constructors -------------------------
        -- | Found an unapplied kind function constructor.
        | ErrorUnappliedKindFun 

        -- | Found a naked sort constructor.
        | ErrorNakedSort
        { errorSort             :: Sort n }

        -- | An undefined type constructor.
        | ErrorUndefinedTypeCtor
        { errorBound            :: Bound n }


        -- Applications -------------------------
        -- | A type application where the thing being applied is not a function.
        | ErrorAppNotFun
        { errorChecking         :: Type n
        , errorFunType          :: Type n
        , errorFunTypeKind      :: Kind n
        , errorArgType          :: Type n }

        -- | A type application where the parameter and argument kinds don't match.
        | ErrorAppArgMismatch   
        { errorChecking         :: Type n
        , errorFunType          :: Type n
        , errorFunKind          :: Kind n
        , errorArgType          :: Type n
        , errorArgKind          :: Kind n }

        -- | A witness implication where the premise or conclusion has an
        --   invalid kind.
        | ErrorWitnessImplInvalid
        { errorChecking         :: Type n
        , errorLeftType         :: Type n
        , errorLeftKind         :: Kind n
        , errorRightType        :: Type n
        , errorRightKind        :: Kind n }


        -- Quantifiers --------------------------
        -- | A forall where the body does not have data or witness kind.
        | ErrorForallKindInvalid
        { errorChecking         :: Type n
        , errorBody             :: Type n
        , errorKind             :: Kind n }


        -- Sums ---------------------------------
        -- | A type sum where the components have differing kinds.
        | ErrorSumKindMismatch
        { errorKindExpected     :: Kind n
        , errorTypeSum          :: TypeSum n
        , errorKinds            :: [Kind n] }
        
        -- | A type sum that does not have effect or closure kind.
        | ErrorSumKindInvalid
        { errorCheckingSum      :: TypeSum n
        , errorKind             :: Kind n }
        deriving Show


-- | Things that can go wrong when checking data type definitions.
data ErrorData n
        -- A duplicate data type constructor name.
        = ErrorDataDupTypeName 
        { errorDataDupTypeName  :: n }

        -- A duplicate data constructor name.
        | ErrorDataDupCtorName
        { errorDataCtorName     :: n }

        -- A data constructor where the type is mis-kinded.
        | ErrorDataBadCtorType
        { errorDataCtorName     :: n
        , errorError            :: Error n }
        deriving Show
