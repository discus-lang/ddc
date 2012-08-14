
module DDC.Core.Simplifier
        ( -- * Description
          Simplifier(..)
        , Transform(..)

          -- * Parsing
        , parseSimplifier

          -- * Baked-in recipies
        , anormalize

          -- * Application
        , applySimplifier
        , applySimplifierX
        , applyTransform
        , applyTransformX)
where
import DDC.Core.Simplifier.Apply
import DDC.Core.Simplifier.Base
import DDC.Core.Simplifier.Parser
import DDC.Core.Simplifier.Recipe
