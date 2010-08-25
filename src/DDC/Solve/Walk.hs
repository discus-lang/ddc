{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Walking over the type graph to find various things.
module DDC.Solve.Walk
	( takeHeadDownLeftSpine
	, takeAppsDownLeftSpine
	, getClassDownLeftSpine)
where
import Type.Base
import Type.State
import Type.Class
import DDC.Type

-- | Walk down the left spine of this type to find the type in the bottom
--   left node (if there is one)
--
--   For example, suppose the graph holds a type like:
--	@NApp (NApp (NCon tc) cid1) cid2@
--	
--   Then starting from the cid of the outermost `NApp`, we'll walk down the
--   left spine until we find @(TCon tc)@, then return @cid2@.
--
--   If the node at the bottom of the spine hasn't been unified, then it'll
--   be a Nothing, so return that instead.
--
takeHeadDownLeftSpine 
	:: ClassId 
	-> SquidM (Maybe Class)
	
takeHeadDownLeftSpine cid1
 = do	Just cls1	<- lookupClass cid1

	case classUnified cls1 of
	 Just (NApp cid11 cid12)	
	   -> do Just cls11	<- lookupClass cid11
		 case classUnified cls11 of
			Just NCon{}	
			 -> do	Just cls12	<- lookupClass cid12
				return $ Just cls12
				
			_	-> takeHeadDownLeftSpine cid11

	 _	-> return $ Nothing


-- | Starting an outermost application node, trace the graph and return
--	a list of the parts being applied. Eg, tracing the following
--	structure from the graph gives @[t1, t2, t3]@.
--
--  @
--         app
--        /   \\
--      app     t3
--     /   \\
--    t1    t2
--  @
--
-- If and of the nodes have `Nothing` for their type, then return `Nothing`.
--
takeAppsDownLeftSpine
	:: ClassId
	-> SquidM (Maybe [ClassId])
	
takeAppsDownLeftSpine cid
 = do	Just cls <- lookupClass cid
	case classUnified cls of
	 Just (NApp cid11 cid12)
	  -> do	mtsLeft	<- takeAppsDownLeftSpine cid11
		case mtsLeft of
			Just cidsLeft	-> return $ Just (cidsLeft ++ [cid12])
			Nothing		-> return Nothing
			
	 Just NCon{}
	  -> 	return	$ Just [cid]
	
	 Just NBot{}
	  ->	return	$ Just [cid]
	
	 _ -> 	return Nothing


-- | Walk down the left spine of this type to find the type in the bottom 
--	left node (if there is one)
--
--	For example, if the graph holds a type like:
--	   @TApp (TApp (TCon tc) t1) t2@
--	
--	Then starting from the cid of the outermost `TApp`, we'll walk down 
--	the left spine until we find @(TCon tc)@, and return that.
--
--	If the node at the bottom of the spine hasn't been unified, then
--	It'll be a `Nothing`, so return that instead.
--
getClassDownLeftSpine :: ClassId -> SquidM Class
getClassDownLeftSpine cid
 = do	Just cls	<- lookupClass cid
	case classUnified cls of
	 Just (NApp cid1 _)	
		-> getClassDownLeftSpine cid1

	 _	-> return cls
