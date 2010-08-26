
module DDC.Solve.State
	( module DDC.Solve.State.Base
	, module DDC.Solve.State.InstanceInfo
	, module DDC.Solve.State.Squid
	, module DDC.Solve.State.Naming
	, module DDC.Solve.State.Sink
	, module DDC.Solve.State.Merge
	, module DDC.Solve.State.Graph
	, lookupSourceOfNode)
where
import DDC.Solve.State.Base
import DDC.Solve.State.Squid
import DDC.Solve.State.InstanceInfo
import DDC.Solve.State.Naming
import DDC.Solve.State.Sink
import DDC.Solve.State.Merge
import DDC.Solve.State.Graph
	
import Type.Location
import Data.Maybe


-- | Get the source of some effect, given the class that contains it.
--	The cids in the provided effect must be in canonical form, 
--	but the cids in the class don't need to be.
--	If there are multiple sources in the class then just take the first one.
lookupSourceOfNode
	:: Node
	-> Class 
	-> SquidM (Maybe TypeSource)

lookupSourceOfNode nEff cls
 = do	tsSrcs	<- mapM sinkCidsInNodeFst $ classTypeSources cls
	return 	$ listToMaybe
		$ [nodeSrc	| (nodeEff,  nodeSrc)	<- tsSrcs
				, nodeEff == nEff]

