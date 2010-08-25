
module DDC.Solve.State.Sink
	( sinkVar
	, sinkClassId
	, sinkCidsInNode
	, sinkCidsInType
	, sinkCidsInFetter
	, sinkCidsInNodeFst
	, sinkCidsInFetterFst)
where
import DDC.Solve.State.Squid
import DDC.Solve.Graph
import DDC.Type
import DDC.Var

sinkVar 		:: Var -> SquidM Var
sinkCidsInNode 		:: Node -> SquidM Node
sinkCidsInType 		:: Type -> SquidM Type
sinkCidsInFetter 	:: Fetter -> SquidM Fetter
sinkCidsInNodeFst 	:: (Node, a) -> SquidM (Node, a)	
sinkCidsInFetterFst	:: (Fetter, a) -> SquidM (Fetter, a)
sinkClassId 		:: ClassId -> SquidM ClassId
