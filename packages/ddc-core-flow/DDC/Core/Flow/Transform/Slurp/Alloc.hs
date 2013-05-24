
module DDC.Core.Flow.Transform.Slurp.Alloc
        (patchAllocRates)
where
import DDC.Core.Flow.Process.Operator


-- | Decide what rates should be used to allocate created vectors.
--   When a vector is being created in a selector context then we need to 
--   use the maximum possible length, which is the outer context instead
--   of the inner one created by the selector.
patchAllocRates :: [Operator] -> [Operator]
patchAllocRates ops
 = let
        -- Build a table of output to input rates for all pack operations.
        packRates       
         = [ (opOutputRate op, opInputRate op)
                | op@OpPack{}   <- ops ]

        getAllocRate rate
         = case lookup rate packRates of
                Just inRate     -> inRate
                _               -> rate

        patchOperator op
         = case op of
                OpCreate{}
                  -> op { opAllocRate = Just $ getAllocRate (opInputRate op) }
                _ -> op

   in   map patchOperator ops
