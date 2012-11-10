
module DDC.Core.Simplifier
        ( -- * Simplifier Specifications
          Simplifier(..)

          -- * Transform Specifications
        , Transform(..)

          -- * Transform Results
        , TransformResult(..)
        , TransformInfo(..)
        , resultDone

          -- * Parsing
        , ModuleName    (..)
        , InlinerTemplates
        , parseSimplifier

          -- * Application
        , applySimplifier
        , applySimplifierX
        , applyTransform
        , applyTransformX)
where
import DDC.Core.Simplifier.Apply
import DDC.Core.Simplifier.Base
import DDC.Core.Simplifier.Parser
