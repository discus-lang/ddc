
module DDC.Core.Tetra.Transform.Curry.Error
        (Error (..))
where
import DDC.Core.Tetra.Prim
import DDC.Type.Exp
import DDC.Base.Pretty
import qualified DDC.Core.Call          as Call


data Error
        -- | Super is not fully named.
        = ErrorSuperUnnamed
        { errorBind     :: Bind Name }

        -- | Super is not in prenex form.
        | ErrorSuperNotPrenex
        { errorBind     :: Bind Name }

        -- | The arity information that we have for a super does not match 
        --   its type. For example, the arity information may say that it
        --   is a function with two parameters, but the type only has a
        --   single one.
        | ErrorSuperArityMismatch
        { errorName     :: Name
        , errorType     :: Type Name
        , errorArity    :: (Int, Int, Int) }

        -- | Type mismatch between the type annotation on a super to call,
        --   and the type we have for it in the callables table.
        | ErrorSuperTypeMismatch
        { errorName     :: Name
        , errorType1    :: Type Name
        , errorType2    :: Type Name }

        -- | We tried to call a super with the wrong call pattern.
        | ErrorSuperCallPatternMismatch
        { errorName      :: Name
        , errorCallType  :: Maybe (Type Name)
        , errorCallCons  :: Maybe [Call.Cons Name]
        , errorCallElims :: [Call.Elim () Name] }
        deriving (Show)


instance Pretty Error where
 ppr err
  = case err of
        ErrorSuperUnnamed b
         -> vcat [ text "Super with binder " 
                        <> (squotes $ ppr b) <> text " lacks a name." ]

        ErrorSuperNotPrenex b
         -> vcat [ text "Super " 
                        <> (squotes $ ppr b) <> text " is not in prenex form." ]

        ErrorSuperArityMismatch n t arity
         -> vcat [ text "Arity information for " 
                        <> ppr n   <> text " does not match its type."
                 , text " type:  " <> ppr t
                 , text " arity: " <> text (show arity) ]

        ErrorSuperTypeMismatch n tAnnot tTable
         -> vcat [ text "Type mismatch for "    
                        <> ppr n   <> text " in super type annotation"
                 , text " type on annotation: " <> ppr tAnnot
                 , text " type of callable:   " <> ppr tTable ]

        ErrorSuperCallPatternMismatch n t cs es
         -> vcat [ text "Call pattern mismatch when calling " <> ppr n
                 , text " call type:  " <> text (show t)
                 , text " call cons:  " <> text (show cs)
                 , text " call elims: " <> text (show es) ]

