
module DDC.Core.Simplifier
        ( -- * Simplifier Specifications
          Simplifier(..)

          -- * Transform Specifications
        , Transform(..)
        , InlinerTemplates
        , NamedRewriteRules

          -- * Transform Results
        , TransformResult(..)
        , TransformInfo(..)
        , resultDone

          -- * Application
        , applySimplifier
        , applySimplifierX)
where
import DDC.Core.Simplifier.Apply
import DDC.Core.Simplifier.Base
