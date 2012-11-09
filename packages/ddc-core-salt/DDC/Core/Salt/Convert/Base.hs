
-- | Things that can go wrong when converting Disciple Core Salt to C-code.
--
--   If we get any of these then the program doesn't map onto the features
--   of the C-language.
module DDC.Core.Salt.Convert.Base
        ( ConvertM      (..)
        , Error(..))
where
import DDC.Core.Salt.Name
import DDC.Core.Pretty
import DDC.Core.Module
import DDC.Core.Exp
import qualified DDC.Control.Monad.Check        as G


-- | Conversion Monad
type ConvertM a x = G.CheckM (Error a) x


-- | Things that can go wrong when converting Disciple Core Salt to
--   to C source text.
data Error a
        -- Modules must contain a top-level letrec.
        = ErrorNoTopLevelLetrec
        { errorModule   :: Module a Name }

        -- | A local variable has an invalid type.
        | ErrorLocalTypeInvalid 
        { errorType     :: Type Name }

        -- | An invalid function definition.
        | ErrorFunctionInvalid
        { errorExp      :: Exp a Name }

        -- | An invalid function parameter.
        | ErrorParameterInvalid
        { errorBind     :: Bind Name }

        -- | An invalid function body.
        | ErrorBodyInvalid
        { errorExp      :: Exp a Name }

        -- | A function body that does not explicitly pass control.
        | ErrorBodyMustPassControl
        { errorExp      :: Exp a Name }

        -- | An invalid statement.
        | ErrorStmtInvalid
        { errorExp      :: Exp a Name }

        -- | An invalid alternative.
        | ErrorAltInvalid
        { errorAlt      :: Alt a Name }

        -- | An invalid RValue.
        | ErrorRValueInvalid
        { errorExp      :: Exp a Name }

        -- | An invalid function argument.
        | ErrorArgInvalid
        { errorExp      :: Exp a Name }

        -- | An invalid primitive call
        | ErrorPrimCallInvalid
        { errorPrimOp   :: PrimOp
        , errorArgs     :: [Exp a Name]}
        deriving Show


instance (Show a, Pretty a) => Pretty (Error a) where
 ppr err
  = case err of
        ErrorNoTopLevelLetrec _mm
         -> vcat [ text "Module does not have a top-level letrec."
                 , empty ]
--                 , text "with:"                         <> align (ppr mm) ]
                -- TODO: need pretty printer for modules.

        ErrorLocalTypeInvalid xx
         -> vcat [ text "Invalid type for local variable."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorFunctionInvalid xx
         -> vcat [ text "Invalid function definition."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorParameterInvalid b
         -> vcat [ text "Invalid function parameter."
                 , empty
                 , text "with:"                                 <+> align (ppr b) ]

        ErrorBodyInvalid xx
         -> vcat [ text "Invalid function body."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorBodyMustPassControl xx
         -> vcat [ text "The final statement in a function must pass control"
                 , text "  You need an explicit return# or fail#."
                 , empty
                 , text "this isn't one: "                      <+> align (ppr xx) ]

        ErrorStmtInvalid xx
         -> vcat [ text "Invalid statement."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorAltInvalid xx
         -> vcat [ text "Invalid case-alternative."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorRValueInvalid xx
         -> vcat [ text "Invalid R-value."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorArgInvalid xx
         -> vcat [ text "Invalid argument."
                 , empty
                 , text "with:"                                 <+> align (ppr xx) ]

        ErrorPrimCallInvalid p xs
         -> vcat [ text "Invalid primCall."
                 , text "   primitive: "                        <+> align (ppr p)
                 , text "        args:  "                       <+> align (ppr xs) ]
