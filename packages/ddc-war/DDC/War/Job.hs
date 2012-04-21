{-# LANGUAGE ExistentialQuantification #-}
module DDC.War.Job where
import BuildBox.Pretty
import BuildBox

-- | A chain of jobs to run one after another.
--   If one job in the chain fails then skip the rest.
data Chain
        = Chain [Job]

-- | A single job to run.
data Job
        =  forall result. Pretty result
        => Job (Build result)

