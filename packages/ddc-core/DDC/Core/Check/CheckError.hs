{-# OPTIONS_HADDOCK hide #-}
-- | Errors produced when checking core expressions.
module DDC.Core.Check.CheckError
        (Error(..))
where
import DDC.Core.Exp
import DDC.Core.Pretty
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

        -- | In let expression, type of binder does not match type of right of binding.
        | ErrorLetMismatch
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }

        -- | Bound region variable is free in the type of the body of a letregion.
        | ErrorLetRegionFree
        { errorChecking         :: Exp a n
        , errorBind             :: Bind n
        , errorType             :: Type n }


instance (Pretty n, Eq n) => Pretty (Error a n) where
 ppr err
  = case err of
        ErrorType err'  -> ppr err'

        ErrorMalformedExp xx
         -> vcat [ text "Core type error."
                 , text "    Found malformed exp: " <> ppr xx ]
        
        ErrorMalformedType xx tt
         -> vcat [ text "Core type error."
                 , text "    Found malformed type: " <> ppr tt
                 , text "           when checking: " <> ppr xx ]

        ErrorAppMismatch xx t1 t2
         -> vcat [ text "Core type error."
                 , text "Cannot apply function " 
                 , text "                 of type: " <> ppr t1
                 , text "     to argument of type: " <> ppr t2
                 , text "          in application: " <> ppr xx ]
         
        ErrorAppNotFun xx t1 t2
         -> vcat [ text "Core type error."
                 , text "Cannot apply non-function"
                 , text "                 of type: " <> ppr t1
                 , text "     to argument of type: " <> ppr t2 
                 , text "          in application: " <> ppr xx ]

        ErrorLamNotPure xx eff
         -> vcat [ text "Core type error."
                 , text "Non-computation abstraction"
                 , text "      has visible effect: " <> ppr eff
                 , text "          but it must be: 0!"
                 , text "           when checking: " <> ppr xx ]
                 
        
        ErrorLamBindNotData xx t1 k1
         -> vcat [ text "Core type error."
                 , text "Parameter of computation abstraction has wrong kind."
                 , text "         type of binder: " <> ppr t1
                 , text "               has kind: " <> ppr k1
                 , text "         but it must be: *" 
                 , text "          when checking: " <> ppr xx ]

        ErrorLamBodyNotData xx b1 t2 k2
         -> vcat [ text "Core type error."
                 , text "Body of abstraction has wrong kind."
                 , text "  in lambda with binder: " <> ppr b1
                 , text "          body has type: " <> ppr t2
                 , text "              with kind: " <> ppr k2
                 , text "         but it must be: *"
                 , text "          when checking: " <> ppr xx ]

        ErrorLamReboundSpec xx b1
         -> vcat [ text "Core type error."
                 , text "Cannot shadow level-1 binder."
                 , text "                 binder: " <> ppr b1
                 , text "  is already in the environment"
                 , text "          when checking: " <> ppr xx ]
        
        ErrorLetMismatch xx b t
         -> vcat [ text "Core type error."
                 , text "Type of binder does not match type of binding in a let expression."
                 , text "         type of binder: " <> ppr (show b)
                 , text "         does not match: " <> ppr (show t)
                 , text "          when checking: " <> ppr xx ]
                 
        ErrorLetRegionFree xx b t
         -> vcat [ text "Core type error."
                 , text "Bound region variable is free in the type of the body of a letregion"
                 , text "                 binder: " <> ppr b
                 , text "             is free in: " <> ppr t
                 , text "          when checking: " <> ppr xx ]
                 
                 