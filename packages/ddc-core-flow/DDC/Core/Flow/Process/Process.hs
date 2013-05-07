
module DDC.Core.Flow.Process.Process
        ( Process       (..))
where
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Prim
import DDC.Core.Exp


-- | A process applies some series operators and produces some 
--   non-series result.
--
--   We get one of these for each top-level series function in the original
--   program.
--
data Process
        = Process
        { -- | Name of whole process.
          --   This is taken from the function name in the original source code.
          processName           :: Name

          -- | Type of element being processed.
        , processType           :: Type Name

          -- | Type parameters to process.
        , processParamTypes     :: [Bind Name]

          -- | Value parameters to process.
        , processParamValues    :: [Bind Name]

          -- | Flow operators in this process.
        , processOperators      :: [Operator] 

          -- | Top-level statements that don't invoke stream operators.
          --   These are typically statements that combine reduction results, 
          --   like the addition in  (fold (+) 0 s1 + fold (*) 1 s1).
          -- 
          --   INVARIANT: the worker functions for stream operators can not 
          --   mention any of of the bound variables.                           -- TODO: check this.
        , processStmts          :: [Lets () Name]

          -- Final result of process.
        , processResult         :: Exp () Name 
        }

