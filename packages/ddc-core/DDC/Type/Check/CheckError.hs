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

        -- | An undefined type variable.
        = ErrorUndefined        
        { errorBound            :: Bound n }

        -- | The kind annotation on the variables does not match the one in the environment.
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

        -- | A witness implication where the premise or conclusion has an invalid kind.
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
        ErrorUnappliedKindFun 
         -> text "Can't take sort of unapplied kind function constructor."
        
        ErrorNakedSort s
         -> text "Can't check a naked sort: " <> ppr s

        ErrorUndefined u
         -> text "Undefined type variable:  " <> ppr u
        
        ErrorVarAnnotMismatch u t
         -> vcat [ text "Type mismatch in annotation."
                 , text "             Variable: "       <> ppr u
                 , text "       has annotation: "       <> (ppr $ typeOfBound u)
                 , text " which conflicts with: "       <> ppr t
                 , text "     from environment." ]
 
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

        ErrorForallKindInvalid tt t k
         -> vcat [ text "Invalid kind for body of quantified type."
                 , text "        the body type: " <> ppr t
                 , text "             has kind: " <> ppr k
                 , text "  but it must be * or @" 
                 , text "        when checking: " <> ppr tt ]
        
        ErrorWitnessImplInvalid tt t1 k1 t2 k2
         -> vcat [ text "Invalid args for witness implication."
                 , text "            left type: " <> ppr t1
                 , text "             has kind: " <> ppr k1
                 , text "           right type: " <> ppr t2
                 , text "             has kind: " <> ppr k2 
                 , text "        when checking: " <> ppr tt ]
                
