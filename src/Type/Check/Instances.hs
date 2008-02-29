
module Type.Check.Instances 
--	( checkInstances )
	
where

{-
import Type.State
import Type.Base
import Type.Exp

debug		= True
trace ss 	= if debug then traceM ss else return ()
-}
-- | Check that all FConstraints in the graph have a corresponding instance.
--	If we don't catch undefined instances here then Core.Dictionary will
--	panic when trying to resolve the overloaded calls.
-- checkInstances :: SquidM ()
-- checkInstances 
