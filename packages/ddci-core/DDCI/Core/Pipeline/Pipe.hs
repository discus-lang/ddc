

module DDCI.Core.Pipeline.Pipe
        ( Target                (..)
        , PipeType              (..))
where


-- Target ---------------------------------------------------------------------
data Target
        = TargetStdout
        | TargetFile
        deriving (Show, Eq)


-- Type -----------------------------------------------------------------------
data PipeType
        = PipeTypeOutput          Target
        | PipeTypeCheck           Target
        deriving (Show, Eq)


-- Witness --------------------------------------------------------------------
data PipeWitness
        = PipeWitnessOutput       Target
        | PipeWitnessCheck        Target
        deriving (Show, Eq)


-- Exp ------------------------------------------------------------------------
data PipeExp
        = PipeExpOutput           Target
        | PipeExpCheck            Target
        deriving (Show, Eq)




