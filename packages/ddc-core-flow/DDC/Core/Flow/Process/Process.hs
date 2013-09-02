
module DDC.Core.Flow.Process.Process
        ( Process       (..)
        , typeOfProcess)
where
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Compounds
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
        }


-- | Take the functional type of a process.
typeOfProcess :: Process -> TypeF
typeOfProcess process
 = let  tBody   = foldr tFun tProcess
                $ map typeOfBind (processParamValues process)

        tQuant  = foldr TForall tBody
                $ processParamTypes process

   in   tQuant
