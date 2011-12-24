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


instance (Pretty n, Eq n) => Pretty (Error a n) where
 ppr err
  = case err of
        ErrorType err'  -> ppr err'

        ErrorMalformedExp xx
         -> vcat [ text "Malformed expression: "        <> ppr xx ]
        
        ErrorMalformedType xx tt
         -> vcat [ text "Found malformed type: "        <> ppr tt
                 , text "       when checking: "        <> ppr xx ]

        ErrorAppMismatch xx t1 t2
         -> vcat [ text "Type mismatch in application." 
                 , text "     Function expects: "       <> ppr t1
                 , text "      but argument is: "       <> ppr t2
                 , text "       in application: "       <> ppr xx ]
         
        ErrorAppNotFun xx t1 t2
         -> vcat [ text "Cannot apply non-function"
                 , text "              of type: "       <> ppr t1
                 , text "  to argument of type: "       <> ppr t2 
                 , text "       in application: "       <> ppr xx ]

        ErrorLamNotPure xx eff
         -> vcat [ text "Impure type abstraction"
                 , text "           has effect: "       <> ppr eff
                 , text "        when checking: "       <> ppr xx ]
                 
        
        ErrorLamBindNotData xx t1 k1
         -> vcat [ text "Function parameter does not have value kind."
                 , text "    The function parameter:"   <> ppr t1
                 , text "                  has kind: "  <> ppr k1
                 , text "            but it must be: *"
                 , text "             when checking: "  <> ppr xx ]

        ErrorLamBodyNotData xx b1 t2 k2
         -> vcat [ text "Result of function does not have value kind."
                 , text "   In function with binder: "  <> ppr b1
                 , text "       the result has type: "  <> ppr t2
                 , text "                 with kind: "  <> ppr k2
                 , text "            but it must be: *"
                 , text "             when checking: "  <> ppr xx ]

        ErrorLamReboundSpec xx b1
         -> vcat [ text "Cannot shadow level-1 binder: " <> ppr b1
                 , text "  when checking: " <> ppr xx ]
        
        ErrorLetMismatch xx b t
         -> vcat [ text "Type mismatch in let-binding."
                 , text "       The binder has type: "  <> ppr b
                 , text "     but the body has type: "  <> ppr t
                 , text "             when checking: "  <> ppr xx ]
                 
        ErrorLetRegionFree xx b t
         -> vcat [ text "Region variable escapes scope of letregion."
                 , text "       The region variable: "  <> ppr b
                 , text "  is free in the body type: "  <> ppr t
                 , text "             when checking: "  <> ppr xx ]
        
        ErrorWAppMismatch ww t1 t2
         -> vcat [ text "Type mismatch in witness application."
                 , text "  Constructor expects: "       <> ppr t1
                 , text "      but argument is: "       <> ppr t2
                 , text "        when checking: "       <> ppr ww]

        ErrorWAppNotCtor ww t1 t2
         -> vcat [ text "Type cannot apply non-constructor witness"
                 , text "              of type: "       <> ppr t1
                 , text "  to argument of type: "       <> ppr t2 
                 , text "        when checking: "       <> ppr ww ]

        ErrorCannotJoin ww w1 t1 w2 t2
         -> vcat [ text "Cannot join witnesses."
                 , text "          Cannot join: "       <> ppr w1
                 , text "              of type: "       <> ppr t1
                 , text "         with witness: "       <> ppr w2
                 , text "              of type: "       <> ppr t2
                 , text "        when checking: "       <> ppr ww ]

        ErrorWitnessNotPurity xx w t
         -> vcat [ text "Witness for a purify does not witness purity."
                 , text "        Witness: "             <> ppr w
                 , text "       has type: "             <> ppr t
                 , text "  when checking: "             <> ppr xx ]

        ErrorWitnessNotEmpty xx w t
         -> vcat [ text "Witness for a forget does not witness emptiness."
                 , text "        Witness: "             <> ppr w
                 , text "       has type: "             <> ppr t
                 , text "  when checking: "             <> ppr xx ]

