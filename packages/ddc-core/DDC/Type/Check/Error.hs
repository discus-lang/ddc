
-- | Errors produced when checking types.
module DDC.Type.Check.Error
        (Error(..))
where
import DDC.Type.Exp


-- | Things that can go wrong when checking the kind of at type.
data Error n

        -- | An undefined type variable.
        = ErrorUndefined        
        { errorBound            :: Bound n }

        -- | An undefined type constructor.
        | ErrorUndefinedTypeCtor
        { errorBound            :: Bound n }

        -- | The kind annotation on the variables does not match the one in the
        --   environment.
        | ErrorVarAnnotMismatch
        { errorBound            :: Bound n
        , errorTypeEnv          :: Type n }

        -- | Found a naked sort constructor.
        | ErrorNakedSort
        { errorSort             :: Sort n }

        -- | Found an unapplied kind function constructor.
        | ErrorUnappliedKindFun 

        -- | A type application where the parameter and argument kinds don't match.
        | ErrorAppArgMismatch   
        { errorChecking         :: Type n
        , errorParamKind        :: Kind n
        , errorArgKind          :: Kind n }

        -- | A type application where the thing being applied is not a function.
        | ErrorAppNotFun
        { errorChecking         :: Type n
        , errorFunType          :: Type n
        , errorFunTypeKind      :: Kind n
        , errorArgType          :: Type n
        , errorArgTypeKind      :: Kind n }

        -- | A type sum where the components have differing kinds.
        | ErrorSumKindMismatch
        { errorKindExpected     :: Kind n
        , errorTypeSum          :: TypeSum n
        , errorKinds            :: [Kind n] }
        
        -- | A type sum that does not have effect or closure kind.
        | ErrorSumKindInvalid
        { errorCheckingSum      :: TypeSum n
        , errorKind             :: Kind n }

        -- | A forall where the body does not have data or witness kind.
        | ErrorForallKindInvalid
        { errorChecking         :: Type n
        , errorBody             :: Type n
        , errorKind             :: Kind n }

        -- | A witness implication where the premise or conclusion has an
        --   invalid kind.
        | ErrorWitnessImplInvalid
        { errorChecking         :: Type n
        , errorLeftType         :: Type n
        , errorLeftKind         :: Kind n
        , errorRightType        :: Type n
        , errorRightKind        :: Kind n }
        
        -- | Cannot infer a type.
        | ErrorCannotInfer
        { errorChecking         :: Type n }
        deriving Show

