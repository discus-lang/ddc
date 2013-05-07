
module DDC.Core.Flow.Exp.Process
        ( Process       (..)
        , Operator      (..)
        , elemTypeOfOperator
        , slurpOperator)
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Flow.Prim
import DDC.Base.Pretty
import DDC.Type.Pretty          ()

-------------------------------------------------------------------------------
-- | A process that applies some stream operators and produces some 
--   non-stream result.
--
--   We get one of these for each top-level stream function in the
--   original program.
--
data Process
        = Process
        { processName           :: Name
        , processType           :: Type Name
        , processParamTypes     :: [Bind Name]
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
        , processResult         :: Exp () Name }


instance Pretty Process where
 ppr p
  = vcat
  $     [ ppr (processName p)
        , text "  element type:  " <> ppr (processType p)
        , text "  parameters:    " <> ppr (processParamValues p) ]
        ++ map (indent 2 . ppr) (processOperators p)


-------------------------------------------------------------------------------
-- | An abstract stream operator.
data Operator
        -- Fold all the elements of a stream.
        = OpFold
        { opRate                :: Type   Name
        , opResult              :: Bind   Name
        , opStream              :: Bound  Name

        , opTypeAcc             :: Type   Name
        , opTypeStream          :: Type   Name

        , opZero                :: Exp () Name

        , opWorkerParamAcc      :: Bind   Name
        , opWorkerParamElem     :: Bind   Name
        , opWorkerBody          :: Exp () Name }


-- | Get the type of stream element that an operator processes.
elemTypeOfOperator :: Operator -> Maybe (Type Name)
elemTypeOfOperator op
 = case op of
        OpFold{}                -> Just $ opTypeStream op


instance Pretty Operator where
 ppr op@OpFold{}
        = vcat
        [ text "Fold"
        , text " rate: "        <> ppr (opRate op) ]


-------------------------------------------------------------------------------
-- | Slurp a stream operator from a let-binding binding.
--   We use this when recovering operators from the source program.
slurpOperator 
        :: Bind Name -> Exp () Name 
        -> Maybe Operator

slurpOperator bResult xx
 -- Slurp a fold# operator.
 | Just ( NameOpFlow OpFlowFold
        , [ XType tRate, XType tAcc, XType tStream
          , xWorker,     xZero,     (XVar _ uStream)])
                                <- takeXPrimApps xx
 , Just ([pAcc, pElem], xBody)  <- takeXLams     xWorker
 = Just $ OpFold
        { opRate                = tRate
        , opResult              = bResult
        , opStream              = uStream 

        , opTypeAcc             = tAcc
        , opTypeStream          = tStream

        , opZero                = xZero

        , opWorkerParamAcc      = pAcc
        , opWorkerParamElem     = pElem
        , opWorkerBody          = xBody }

 | otherwise
 = Nothing
