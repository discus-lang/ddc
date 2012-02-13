
-- | Single step evaluator for the Disciple Core language.
--
--   This is a direct implementation of the operational semantics and is by no
--   means fast, or a substitute for a real interpreter. Programs run with this
--   evaluator will have an asymptotic complexity much worse than if they were
--   compiled. This evaluator is intended for experimenting with the language
--   semantics, and not running actual programs.
module DDC.Core.Eval
        ( StepResult    (..)
        , force
        , step)
where
import DDC.Core.Eval.Step
        
