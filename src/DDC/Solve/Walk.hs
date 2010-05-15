
-- | Walking over the type graph to find various things.
module DDC.Solve.Walk
	(headClassDownLeftSpine)
where
import Type.Exp
import Type.Base
import Type.State
import Type.Class

-- | Walk down the left spine of this type to find the type in the bottom
--   left node (if there is one)
--
--   For example, suppose the graph holds a type like:
--	NApp (NApp (NCon tc) cid1) cid2
--	
--   Then starting from the cid of the outermost NApp, we'll walk down the
--   left spine until we find (TCon tc), then return cid2.
--
--   If the node at the bottom of the spine hasn't been unified, then it'll
--   be a Nothing, so return that instead.
--
headClassDownLeftSpine 
	:: ClassId 
	-> SquidM (Maybe Class)
	
headClassDownLeftSpine cid1
 = do	Just cls1	<- lookupClass cid1

	case classType cls1 of
	 Just (NApp cid11 cid12)	
	   -> do Just cls11	<- lookupClass cid11
		 case classType cls11 of
			Just NCon{}	
			 -> do	Just cls12	<- lookupClass cid12
				return $ Just cls12
				
			_	-> headClassDownLeftSpine cid11

	 _	-> return $ Nothing
