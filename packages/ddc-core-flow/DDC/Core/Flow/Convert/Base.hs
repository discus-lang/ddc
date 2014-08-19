
module DDC.Core.Flow.Convert.Base
        (  ConvertM
        ,  Error (..))
where
import DDC.Base.Pretty
import DDC.Core.Flow.Prim                       as F
import qualified DDC.Control.Monad.Check        as G


-- | Conversion Monad
type ConvertM x = G.CheckM () Error x


-- | Things that can go wrong during the conversion.
data Error
        -- | An invalid name used in a binding position
        = ErrorInvalidBinder F.Name

        -- | A partially applied primitive, such as "Series"
        | ErrorPartialPrimitive F.Name

        -- | An invalid name used for the constructor of an alternative.
        | ErrorInvalidAlt

        -- | Found an unexpected type sum.
        | ErrorUnexpectedSum


instance Pretty Error where
 ppr err
  = case err of
        ErrorInvalidBinder n
         -> vcat [ text "Invalid name used in binder '" <> ppr n <> text "'."]

        ErrorPartialPrimitive n
         -> vcat [ text "Cannot convert primitive " <> ppr n <> text "." ]

        ErrorInvalidAlt
         -> vcat [ text "Invalid alternative." ]

        ErrorUnexpectedSum
         -> vcat [ text "Unexpected type sum."]

