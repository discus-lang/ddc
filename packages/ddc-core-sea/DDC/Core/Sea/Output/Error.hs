
module DDC.Core.Sea.Output.Error
        (Error(..))
where
import DDC.Core.Sea.Output.Name
import DDC.Core.Pretty
import DDC.Core.Module
import DDC.Core.Exp


data Error a
        -- Modules must contain a top-level letrec.
        = ErrorNoTopLevelLetrec
        { errorModule   :: Module a Name }

        -- Function definitions -------
        -- | An invalid function definition.
        | ErrorFunctionInvalid
        { errorExp      :: Exp a Name }

        -- Function bodies ------------
        -- | An invalid function body.
        | ErrorBodyInvalid
        { errorExp      :: Exp a Name }

        -- | A function body that does not explicitly pass control.
        | ErrorBodyMustPassControl
        { errorExp      :: Exp a Name }

        -- RValues --------------------
        -- | An invalid RValue.
        | ErrorRValueInvalid
        { errorExp      :: Exp a Name }

        -- Function arguments ---------
        -- | An invalid functino argument.
        | ErrorArgInvalid
        { errorExp      :: Exp a Name }


instance (Show a, Pretty a) => Pretty (Error a) where
 ppr err
  = case err of
        ErrorNoTopLevelLetrec _mm
         -> vcat [ text "Module does not have a top-level letrec."
                 , empty ]
--                 , text "with:"                         <> align (ppr mm) ]
                -- TODO: need pretty printer for modules.

        ErrorFunctionInvalid xx
         -> vcat [ text "Invalid function definition."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorBodyInvalid xx
         -> vcat [ text "Invalid function body."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorBodyMustPassControl xx
         -> vcat [ text "The final statement in a function must pass control"
                 , text "  You need an explicit return# or fail#."
                 , empty
                 , text "this isn't one: "                      <+> align (ppr xx) ]

        ErrorRValueInvalid xx
         -> vcat [ text "Invalid R-value."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorArgInvalid xx
         -> vcat [ text "Invalid argument."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]
