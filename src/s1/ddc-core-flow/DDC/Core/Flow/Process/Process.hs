
module DDC.Core.Flow.Process.Process
        ( Process       (..))
where
import DDC.Core.Flow.Context
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp


-- | A process is a graph of series operators that read from some parameter
--   series and write to some accumulators.
data Process
        = Process
        { -- | Name of whole process.
          --   This is taken from the function name in the original
          --   source code.
          processName           :: Name

          -- | Proc type
        , processProcType       :: TypeF

          -- | Rate of process loop
        , processLoopRate       :: TypeF

          -- | Parameters to process.
          --   These are the parameters of the original function, with flag being true for types.
        , processParamFlags     :: [(Bool, BindF)]

          -- | Flow context in this process.
          --   This contains a ContextRate entry for all the Rate variables
          --   in the parameters, along with an entry for all the nested
          --   contexts introduced by the process itself.
        , processContext        :: Context
        }

