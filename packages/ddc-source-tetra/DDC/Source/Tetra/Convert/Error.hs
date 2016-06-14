{-# LANGUAGE UndecidableInstances #-}
module DDC.Source.Tetra.Convert.Error
        (ErrorConvert (..))
where
import DDC.Source.Tetra.Pretty
import DDC.Source.Tetra.Exp.Generic


data ErrorConvert l
        -- | Cannot convert sugar expression to core.
        = ErrorConvertCannotConvertSugarExp  (GExp l)

        -- | Cannot convert sugar let bindings to core.
        | ErrorConvertCannotConvertSugarLets (GLets l)


instance (PrettyLanguage l)
      => Pretty (ErrorConvert l) where
 ppr err
  = case err of
        ErrorConvertCannotConvertSugarExp xx
         -> vcat [ text "Cannot desugar expression"
                 , indent 2 $ ppr xx ]

        ErrorConvertCannotConvertSugarLets xx
         -> vcat [ text "Cannot desugar let-bindings"
                 , indent 2 $ ppr xx ]

