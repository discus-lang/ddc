{-# OPTIONS_HADDOCK hide #-}
-- | Errors produced when checking types.
module DDC.Type.Check.CheckError
        (Error(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Pretty


-- Error ------------------------------------------------------------------------------------------
-- | Type errors.
data Error n

        -- | Kinds of paramter and arg don't match when checking type application.
        = ErrorAppArgMismatch   
        { errorChecking         :: Type n
        , errorParamKind        :: Kind n
        , errorArgKind          :: Kind n }

        -- | Tried to apply a non-functional type to an argument.
        | ErrorAppNotFun
        { errorChecking         :: Type n
        , errorFunType          :: Type n
        , errorFunTypeKind      :: Kind n
        , errorArgType          :: Type n
        , errorArgTypeKind      :: Kind n }

        -- | Found an unapplied kind function constructor.
        | ErrorUnappliedKindFun 

        -- | Tried to check a sort.
        | ErrorNakedSort
        { errorSort             :: Sort n }

        -- | Found an undefined variable.
        | ErrorUndefined        
        { errorBound            :: Bound n }
        
        -- | Found types with multiple kinds in a sum.
        --   If the kind hasn't been attached to the `TypeSum` yet then it may
        --   be holding the placeholder value (tBot sComp).
        | ErrorSumKindMismatch
        { errorKindExpected     :: Kind n
        , errorTypeSum          :: TypeSum n
        , errorKinds            :: [Kind n] }
        
        -- | Found a sum with an invalid kind.
        --   Sums can only have effect or closure kind.
        | ErrorSumKindInvalid
        { errorCheckingSum      :: TypeSum n
        , errorKind             :: Kind n }

        -- | Found a witness implication whose types have invalid kinds.
        | ErrorWitnessImplInvalid
        { errorChecking         :: Type n
        , errorLeftType         :: Type n
        , errorLeftKind         :: Kind n
        , errorRightType        :: Type n
        , errorRightKind        :: Kind n }
        deriving Show


instance (Eq n, Pretty n) => Pretty (Error n) where
 ppr err
  = case err of
        ErrorAppArgMismatch tt t1 t2
         -> vcat [ text "Core type mismatch in application."
                 , text "             type: " <> ppr t1
                 , text "   does not match: " <> ppr t2
                 , text "   in application: " <> ppr tt ]
         
        ErrorAppNotFun tt t1 k1 t2 k2
         -> vcat [ text "Core type mismatch in application."
                 , text "     cannot apply type: " <> ppr t2
                 , text "               of kind: " <> ppr k2
                 , text "  to non-function type: " <> ppr t1
                 , text "               of kind: " <> ppr k1
                 , text "         in appliction: " <> ppr tt]
                
        ErrorUnappliedKindFun 
         -> text "Can't take sort of unapplied kind function constructor."
        
        ErrorNakedSort s
         -> text "Can't check a naked sort: " <> ppr s

        ErrorUndefined u
         -> text "Undefined type variable when checking type: " <> ppr u
         
        ErrorSumKindMismatch k ts ks
         -> vcat 
              $  [ text "Core type mismatch in sum."
                 , text " found multiple types: " <> ppr ts
                 , text " with differing kinds: " <> ppr ks ]
                 ++ (if k /= tBot sComp
                        then [text "        expected kind: " <> ppr k ]
                        else [])
                
        ErrorSumKindInvalid ts k
         -> vcat [ text "Invalid kind for type sum."
                 , text "         the type sum: " <> ppr ts
                 , text "             has kind: " <> ppr k
                 , text "  but it must be ! or $" ]
        
        ErrorWitnessImplInvalid tt t1 k1 t2 k2
         -> vcat [ text "Invalid args for witness implication."
                 , text "            left type: " <> ppr t1
                 , text "             has kind: " <> ppr k1
                 , text "           right type: " <> ppr t2
                 , text "             has kind: " <> ppr k2 
                 , text "        when checking: " <> ppr tt ]
                
