
module DDC.Core.Flow.Transform.Prep
        (prepModule)
where
import DDC.Core.Transform.TransformX
import DDC.Core.Flow.Name
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp


-- | Prepare a module for lowering.
--   We need all worker functions passed to flow operators to be eta-expanded
--   and for their parameters to have real names.
--
prepModule :: Module () Name -> Module () Name
prepModule mm
        = transformUpX' prepX mm


prepX :: Exp () Name -> Exp () Name
prepX xx
        | Just (XVar _ u, [XType tK, XType tA, XType tB, xF, xZ, xS])     
                                                <- takeXApps xx
        , UPrim (NameFlowOp FlowOpFold) _       <- u
        = let  
                -- TODO: do more general type-directed eta expansion.
                xF'     = xLams ()    [BAnon tA,        BAnon tB] 
                        $ xApps () xF [XVar () (UIx 1), XVar () (UIx 0)]

          in    xApps () (XVar () u)
                         [ XType tK, XType tA, XType tB, xF', xZ, xS ]

        | otherwise
        = xx


