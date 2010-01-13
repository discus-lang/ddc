
module Type.Diagnose 
	( traceFetterSource
	, traceEffectSource)
where
import Type.Error
import Type.State
import Type.Base
import Type.Class
import Type.Location
import Type.Exp
import Shared.VarPrim
import Util

-----
debug	= True
trace ss	
 = if debug 
 	then traceM ss 
 	else return ()

	
-- Trace Fetters -----------------------------------------------------------------------------------
-- | Trace up the type graph to find the original source of this fetter.
--	We follow the SICrushedF breadcrumbs until we find why the original,
--	compound fetter was introduced into the type graph.
--
traceFetterSource 
	:: Var		-- the var of the fetter we're tracing
	-> ClassId	-- the class we're currently looking in
	-> SquidM (Maybe TypeSource)
	
traceFetterSource vC cid
 = do	Just c@(Class 
		{ classFetterSources	= fetterSources
		, classTypeSources	= typeSources })
 		<- lookupClass cid
 	
	trace	$ "*   traceFetterSource" <> vC <> cid % "\n"
		% "    fetterSources:\n"
		%> "\n" %!% fetterSources % "\n\n"
		% "    typeSources:\n"
		%> "\n" %!% typeSources % "\n\n"
	
  	traceFetterSource' vC fetterSources
	
traceFetterSource' vC []
	= return Nothing
	
traceFetterSource' vC (node : nodes)
{-
	-- fetter is the result of crushing some other fetter,
	--	so trace that instead
	| ( FConstraint vC' _
	  , TSI (SICrushedF cidF (FConstraint vC2 _)))	
			<- node
	, vC == vC'
	= traceFetterSource vC2 cidF
-}	
	-- purity fetter is the result of purifying something else
{-	| ( TFetter (FConstraint vC' _)
	  , TSI (SIPurify cidF (TClass KEffect cidE)) )	
	  		<- node
	, vC == vC'
	, vC == primPure
	= traceFetterSource vC cidE
-}

	| ( FConstraint vC' _
	  , TSI (SICrushedFS cid eff ts) )
	  		<- node
	, vC' == vC
	= return $ Just ts
		
	-- found the root cause
	| (FConstraint vC' _
	  , ts@(TSV sv))
			<- node
	, vC == vC'
	= return $ Just ts
	
	-- keep looking
	| otherwise
	= traceFetterSource' vC nodes
 
 
-- Trace Effects -----------------------------------------------------------------------------------

-- | Trace up the type graph to find the original source of this effect.
--	We follow the SICrushedE breadcrumbs until we find why the original,
--	compound effect was introduced into the type graph.
--	
--	This is a bit more complicated than traceFetterSource above because the effect
--	that we're looking for might be in a sum..
--
traceEffectSource
	:: Var		-- the var of the effect we're tracing
	-> Type		-- the type that the effect is acting on
	-> ClassId	-- the class we're currently looking in
	-> SquidM (Maybe TypeSource)
	
traceEffectSource vE tE cid
 = do	Just c@(Class { classTypeSources = nodes })
 		<- lookupClass cid
		
	trace 	$ "*   traceEffectSource " <> vE <> tE <> cid % "\n"
	 	% "    nodes:\n"	
		%> ("\n" %!% nodes % "\n\n")

	traceES_nodes vE tE nodes
	
traceES_nodes vC tE []
	= return Nothing

-- see if the effect we're looking for matches this one
traceES_nodes vC tE 
	(node@(TEffect vCN [tEN], ts) : nodes)
 = do	
	-- we need to sink the classId here because the 
	--	nodes won't have the current classId subsitution applied to them.
	tE'	<- updateVC tE
	tEN'	<- updateVC tEN
		
	if tE' == tEN'

	      -- this effect matches, so follow it up..
	 then traceES_follow vC tE (node : nodes)

	      -- some other effect, keep looking.
	 else traceES_nodes  vC tE nodes

-- see if the effect we're looking for is inside this sum
traceES_nodes vC tE
	(node@(TSum kE es, ts) : nodes)
 | kE	== kEffect
 = do	
	tEff	<- updateVC (TEffect vC [tE])
 	es'	<- updateVC es
	
	if elem tEff es'
	 then traceES_follow vC tE (node : nodes)
	 else traceES_nodes vC tE nodes

traceES_nodes vC tE (node : nodes)
	= traceES_nodes vC tE nodes

traceES_follow vC tE (node : nodes)

	-- effect is the result of crushing some other effect
	--	so trace this new effect instead.
	| ( eff
	  , TSI (SICrushedES cid2 (TEffect vC2 [tE2]) _)) <- node
	= traceEffectSource vC2 tE2 cid2
		 
	-- we've found the source for this single effect
	| ( eff
	  , ts) <- node
	= return $ Just ts

 