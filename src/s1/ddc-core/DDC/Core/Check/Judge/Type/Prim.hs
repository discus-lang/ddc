{-# OPTIONS_HADDOCK hide #-}
module DDC.Core.Check.Judge.Type.Prim
        ( checkPrim
        , shapeOfPrim)
where
import DDC.Core.Check.Judge.Type.Base
import qualified DDC.Type.Sum           as Sum


checkPrim :: Checker a n
checkPrim !_table !ctx !_mode !_demand (XPrim a p)
 = do
        returnX a
                (\z -> XPrim z p)
                (shapeOfPrim p)
                (Sum.empty kEffect)
                ctx


checkPrim _ _ _ _ _
 = error "ddc-core.checkPrim: no match"



shapeOfPrim :: Prim -> Type n
shapeOfPrim p
 = case p of
        PElaborate
         -> tForall kData
         $  \tVal -> tVal

        PProject _
         -> tForalls [kData, kData]
         $  \[tObj, tResult] -> tObj `tFun` tResult

        _ -> error "shapeOfPrim: no match"