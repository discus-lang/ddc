{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Error.ErrorType where
import DDC.Core.Exp
import DDC.Type.Universe


-- | Things that can go wrong when checking the kind of at type.
data ErrorType n
        -- Generic Problems ---------------------
        -- | Tried to check a type using the wrong universe,
        --   for example: asking for the kind of a kind.
        = ErrorTypeUniverseMalfunction
        { errorTypeType         :: Type n
        , errorTypeUniverse     :: Universe }

        -- | Generic kind mismatch.
        | ErrorTypeMismatch
        { errorTypeUniverse     :: Universe
        , errorTypeInferred     :: Type n
        , errorTypeExpected     :: Type n
        , errorTypeChecking     :: Type n }

        -- | Cannot construct infinite type.
        | ErrorTypeInfinite
        { errorTypeVar          :: Type n
        , errorTypeBind         :: Type n }

        -- Variables ----------------------------
        -- | An undefined type variable.
        | ErrorTypeUndefined
        { errorTypeBound        :: Bound n }


        -- Constructors -------------------------
        -- | Found an unapplied kind function constructor.
        | ErrorTypeUnappliedKindFun

        -- | Found a naked sort constructor.
        | ErrorTypeNakedSort
        { errorTypeSort         :: Sort n }

        -- | An undefined type constructor.
        | ErrorTypeUndefinedTypeCtor
        { errorTypeBound        :: Bound n }


        -- Applications -------------------------
        -- | A type application where the thing being applied is not a function.
        | ErrorTypeAppNotFun
        { errorTypeChecking     :: Type n
        , errorTypeFunType      :: Type n
        , errorTypeFunTypeKind  :: Kind n
        , errorTypeArgType      :: Type n }

        -- | A type application where the parameter and argument kinds don't match.
        | ErrorTypeAppArgMismatch
        { errorTypeChecking     :: Type n
        , errorTypeFunType      :: Type n
        , errorTypeFunKind      :: Kind n
        , errorTypeArgType      :: Type n
        , errorTypeArgKind      :: Kind n }

        -- | A witness implication where the premise or conclusion has an
        --   invalid kind.
        | ErrorTypeWitnessImplInvalid
        { errorTypeChecking     :: Type n
        , errorTypeLeftType     :: Type n
        , errorTypeLeftKind     :: Kind n
        , errorTypeRightType    :: Type n
        , errorTypeRightKind    :: Kind n }


        -- Quantifiers --------------------------
        -- | A forall where the body does not have data or witness kind.
        | ErrorTypeForallKindInvalid
        { errorTypeChecking     :: Type n
        , errorTypeBody         :: Type n
        , errorTypeKind         :: Kind n }


        -- Sums ---------------------------------
        -- | A type sum where the components have differing kinds.
        | ErrorTypeSumKindMismatch
        { errorTypeKindExpected :: Kind n
        , errorTypeTypeSum      :: TypeSum n
        , errorTypeKinds        :: [Kind n] }

        -- | A type sum that does not have effect or closure kind.
        | ErrorTypeSumKindInvalid
        { errorTypeCheckingSum  :: TypeSum n
        , errorTypeKind         :: Kind n }
        deriving Show


