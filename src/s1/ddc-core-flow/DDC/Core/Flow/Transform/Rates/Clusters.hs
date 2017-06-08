{-# LANGUAGE CPP #-}
module DDC.Core.Flow.Transform.Rates.Clusters
    (cluster)
 where

#if DDC_FLOW_HAVE_LINEAR_SOLVER
import DDC.Core.Flow.Transform.Rates.Clusters.Linear

cluster = solve_linear

#else
import DDC.Core.Flow.Transform.Rates.Clusters.Greedy

cluster = cluster_greedy

#endif

