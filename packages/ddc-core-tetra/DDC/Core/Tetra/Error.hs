
module DDC.Core.Tetra.Error 
        (Error (..))
where
import DDC.Core.Tetra.Prim
import DDC.Type.Pretty
import DDC.Type.Exp


-- | Fragment specific errors.
data Error a
        -- | Main module does not export a 'main' function.
        = ErrorMainMissing

        -- | Main module exports a 'main' function in an invalid way.
        | ErrorMainInvalidMode

        -- | Main module exports a 'main' function with an invalid type.
        | ErrorMainInvalidType (Type Name)
        deriving Show


instance Pretty (Error a) where
 ppr ErrorMainMissing
  = vcat [ text "Main module does not export a 'main' function." ]

 ppr (ErrorMainInvalidMode)
  = vcat [ text "Invalid export mode for main function in Main module." ]

 ppr (ErrorMainInvalidType t)
  = vcat [ text "Invalid type of main function in Main module."
         , text "  Type of main function: "  <> ppr t
         , text "  is not an instance of: [e : Effect]. Unit -> S e Unit" ]
