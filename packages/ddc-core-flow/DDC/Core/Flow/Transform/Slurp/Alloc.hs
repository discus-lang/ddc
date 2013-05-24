
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

        -- Fix the number of nested contexts to some finite number so we
        -- don't end up diverging if there is a loop in the  list of
        -- operator descriptions.
        maxNestedContexts = 1000 :: Int

        getAllocRate 0 _rate
         = error "ddc-core-flow.patchAllocRates: too many nested contexts"

        getAllocRate n rate
         = case lookup rate packRates of
                Just inRate     -> getAllocRate (n - 1) inRate
                _               -> rate

        patchOperator op@OpCreate{}
         = op { opAllocRate = Just $ getAllocRate maxNestedContexts (opInputRate op) }

        patchOperator op
         = op

   in   map patchOperator ops
