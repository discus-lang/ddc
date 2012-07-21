
module DDC.Core.Lite.Convert.Base
        (  ConvertM(..)
        ,  Error (..))
where
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Check                   (AnTEC(..))
import qualified DDC.Type.Check.Monad   as G
import qualified DDC.Core.Lite.Name     as L


-- | Conversion Monad
type ConvertM a x = G.CheckM (Error a) x


-- | Things that can go wrong during the conversion.
--   All but the first should not happen with type checked code.
data Error a
        -- | The 'Main' module has no 'main' function.
        = ErrorMainHasNoMain

        -- | Found unexpected AST node, like LWithRegion.
        | ErrorMalformed String

        -- | The program is definately not well typed.
        | ErrorMistyped  (Exp (AnTEC a L.Name) L.Name)

        -- | The program wasn't in a-normal form.
        | ErrorNotNormalized

        -- | The program has bottom type annotations.
        | ErrorBotAnnot

        -- | Found an unexpected type sum.
        | ErrorUnexpectedSum

        -- | An invalid name used in a binding position
        | ErrorInvalidBinder L.Name

        -- | An invalid name used in a bound position
        | ErrorInvalidBound  (Bound L.Name)

        -- | An invalid name used for the constructor of an alternative.
        | ErrorInvalidAlt



instance Show a => Pretty (Error a) where
 ppr err
  = case err of
        ErrorMalformed str
         -> vcat [ text "Module is malformed."
                 , text str ]

        ErrorMistyped xx
         -> vcat [ text "Module is mistyped." <> (text $ show xx) ]

        ErrorNotNormalized
         -> vcat [ text "Module is not in a-normal form."]

        ErrorBotAnnot
         -> vcat [ text "Found bottom type annotation."]

        ErrorUnexpectedSum
         -> vcat [ text "Unexpected type sum."]

        ErrorInvalidBinder n
         -> vcat [ text "Invalid name used in bidner " <> ppr n ]

        ErrorInvalidBound n
         -> vcat [ text "Invalid name used in bound occurrence " <> ppr n ]

        ErrorInvalidAlt
         -> vcat [ text "Invalid alternative" ]

        ErrorMainHasNoMain
         -> vcat [ text "Main module has no 'main' function" ]



