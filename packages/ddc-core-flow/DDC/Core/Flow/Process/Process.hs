
module DDC.Core.Flow.Process.Process
        (Process       (..))
where
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Context
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp


-- | A process applies some series operators and produces some non-series
--   result.
--
--   We get one of these for each top-level series function in the
--   original program.
data Process
        = Process
        { -- | Name of whole process.
          --   This is taken from the function name in the original
          --   source code.
          processName           :: Name

          -- | Type parameters to process.
          --   These are the type parameters of the original function.
        , processParamTypes     :: [BindF]

          -- | Value parameters to process.
          --   These are the value parameters of the original function.
        , processParamValues    :: [BindF]

          -- | Flow contexts in this process.
          --   This contains a ContextRate entry for all the Rate variables
          --   in the parameters, along with an entry for all the nested
          --   contexts introduced by the process itself.
        , processContexts       :: [Context]

          -- | Flow operators in this process.
        , processOperators      :: [Operator] 

          -- Type of process result
        , processResultType     :: TypeF

          -- Final result of process.
        , processResultExp      :: ExpF
        }

