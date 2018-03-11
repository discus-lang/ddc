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
        , RuntimeLinkStrategy (..)

          -- * Tetra stages
        , sourceLoadText
        , tetraLoadText
        , tetraToSalt

          -- * Flow stages
        , stageFlowLoad
        , stageFlowPrep
        , stageFlowRate
        , stageFlowLower
        , stageFlowWind
        , stageFlowToTetra

        , stageMachineLoad
        , stageMachinePrep
        , stageMachineOutputSlurp
        , stageMachineOutputFused

          -- * Salt stages
        , saltLoadText
        , saltSimplify
        , saltToLlvm
        , saltCompileViaLlvm)
where
import DDC.Driver.Config
import DDC.Driver.Stage.Tetra
import DDC.Driver.Stage.Flow
import DDC.Driver.Stage.Machine
import DDC.Driver.Stage.Salt
