-- | Compiler stages.
--
--     A compiler stage is a sequence of standard transformations.
--     Each of the individual transformations are expressed as a pipeline from 
--     "DDC.Build.Pipeline". The stages here run several pipelines each,
--     and contain the code that can dump the intermediate program after
--     each transformation.
--
module DDC.Driver.Stage
        ( Config        (..)
        , ViaBackend    (..)

          -- * Flow stages
        , stageFlowLoad
        , stageFlowPrep
        , stageFlowRate
        , stageFlowLower
        , stageFlowWind

          -- * Lite stages
        , stageLiteLoad
        , stageLiteOpt
        , stageLiteToSalt

          -- * Salt stages
        , stageSaltOpt
        , stageSaltToC
        , stageSaltToLLVM
        , stageCompileSalt

          -- * LLVM stages
        , stageCompileLLVM)
where
import DDC.Driver.Config
import DDC.Driver.Stage.Flow
import DDC.Driver.Stage.Lite
import DDC.Driver.Stage.Salt
