
module DDC.Source.Tetra.Convert.Error
        (ErrorConvert (..))
where
import DDC.Source.Tetra.Pretty
import qualified DDC.Source.Tetra.Exp.Annot   as S


data ErrorConvert a
        -- | Cannot convert sugar expression to core.
        = ErrorConvertCannotConvertSugarExp  (S.Exp a)

        -- | Cannot convert sugar let bindings to core.
        | ErrorConvertCannotConvertSugarLets (S.Lets a)


instance Pretty a => Pretty (ErrorConvert a) where
 ppr err
  = case err of
        ErrorConvertCannotConvertSugarExp xx
         -> vcat [ text "Cannot desugar expression"
                 , indent 2 $ ppr xx ]

        ErrorConvertCannotConvertSugarLets xx
         -> vcat [ text "Cannot desugar let-bindings"
                 , indent 2 $ ppr xx ]

