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
         -> tForall   kData $ \tVal -> tVal

        PTuple ls
         -> tForalls (replicate (length ls) kData)
          $ \tsParam -> foldr tFun
                         (tTuple  [ (l, t) | l <- ls | t <- tsParam])
                         tsParam

        PRecord ls
         -> tForalls (replicate (length ls) kData)
          $ \tsParam -> foldr tFun
                         (tRecord [ (l, t) | l <- ls | t <- tsParam ])
                         tsParam

        PVariant _l
         -> tForalls [kData, kData]
          $ \[tObj, tField]  -> tField `tFun` tObj

        PProject _l
         -> tForalls [kData, kData]
         $  \[tObj, tField] -> tObj    `tFun` tField

