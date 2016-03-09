
module DDC.Core.Tetra.Transform.Curry.Error
        (Error (..))
where
import DDC.Core.Tetra.Prim
import DDC.Type.Exp
import DDC.Base.Pretty


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
        deriving (Eq, Show)


instance Pretty Error where
 ppr err
  = case err of
        ErrorSuperArityMismatch n t arity
         -> vcat [ text "Arity information for " 
                        <> ppr n   <> text " does not match its type."
                 , text " type:  " <> ppr t
                 , text " arity: " <> text (show arity) ]

        ErrorSuperUnnamed b
         -> vcat [ text "Super with binder " 
                        <> (squotes $ ppr b) <> text " lacks a name." ]

        ErrorSuperNotPrenex b
         -> vcat [ text "Super " 
                        <> (squotes $ ppr b) <> text " is not in prenex form." ]


