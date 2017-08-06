{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE UndecidableInstances #-}
module DDC.Source.Tetra.Convert.Error
        (ErrorConvert (..))
where
import DDC.Data.SourcePos
import DDC.Source.Tetra.Pretty
import DDC.Source.Tetra.Exp.Generic


-- | Things that can go wrong when converting source to core.
data ErrorConvert l
        -- | Cannot convert sugared expression to core.
        --   This should have been desugared in a prior pass.
        = ErrorConvertSugaredExp    (GExp l)

        -- | Cannot convert sugared let bindings to core.
        --   This should have been desugared in a prior pass.
        | ErrorConvertSugaredLets   (GLets l)

        -- | Cannot convert sugared clause to core.
        --   This should have been desugared in a prior pass.
        | ErrorConvertSugaredClause (GClause l)

        -- | Found multiple type signatures for the same binder.
        --   This should have been desugared in a prior pass.
        | ErrorMultipleSignatures        SourcePos (GXBindVar l)

        -- | Type signature lacks associated value-level binding.
        | ErrorTypeSignatureLacksBinding SourcePos (GXBindVar l)


instance (PrettyLanguage l) => Pretty (ErrorConvert l) where
 ppr = pprError

pprError (ErrorConvertSugaredExp xx)
 = vcat [ text "Cannot convert sugared expression"
        , indent 2 $ ppr xx ]

pprError (ErrorConvertSugaredLets xx)
 = vcat [ text "Cannot convert sugared let-bindings"
        , indent 2 $ ppr xx ]

pprError (ErrorConvertSugaredClause l)
 = vcat [ text "Cannot convert sugared let-bindings"
        , indent 2 $ ppr l ]

pprError (ErrorMultipleSignatures sp b)
 = vcat [ ppr sp
        , text "Multiple type signatures specified for " <> ppr b ]

pprError (ErrorTypeSignatureLacksBinding sp b)
 = vcat [ ppr sp
        , text "Type signature lacks associated binding " <> ppr b]
