{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Float.Stats where
import DDC.Var
import DDC.Main.Pretty


-- | Status about how many bindings were moved / notmoved
--   These are lists instead of sets, to reduce the overhead of collecting up the stats.
data Stats
	= Stats
	{ -- total number of bindings inspected
	  statsTotalBindings	:: Int
	
	  -- bindings that were not moved because they were in the no-float table
	, statsNotMovedNoFloat	:: [Var]

	  -- bindings that were not moved because they had multiple uses
	, statsNotMovedMultiUse	:: [Var]

	  -- bindings that were not moved because their use was at a deeper lambda level
	, statsNotMovedDeepLambda :: [Var]
	
	  -- bindings that were not moved because they had top-level effects.
	, statsNotMovedTopLevel	:: [Var]
	
	  -- bindings that were not moved because they had no uses.
	, statsNotMovedNoUses	:: [Var]
	
	  -- bindings that were not moved, and all their uses were directly under unboxings
	  -- .. not all of these are total failures, because the stmt might not have boxed the value itself.
	, statsMissedUnboxing	:: [Var]
	
	  -- bindings that were successfully moved
	, statsMoved		:: [Var] 

	  -- calls to force successfully shared
	, statsSharedForcings	:: [Var]

	  -- unboxings successfully shared
	, statsSharedUnboxings	:: [Var] }

	
-- empty stats table
statsZero
	= Stats
	{ statsTotalBindings		= 0
	, statsNotMovedNoFloat		= []
	, statsNotMovedMultiUse		= []
	, statsNotMovedDeepLambda	= []
	, statsNotMovedTopLevel		= []
	, statsNotMovedNoUses		= []
	, statsMissedUnboxing		= []
	, statsMoved			= [] 
	
	, statsSharedForcings		= []
	, statsSharedUnboxings		= [] }

-- horror
statsTotalBindings_		= (statsTotalBindings, 		\x s -> s { statsTotalBindings 		= x})
statsNotMovedNoFloat_		= (statsNotMovedNoFloat, 	\x s -> s { statsNotMovedNoFloat	= x})
statsNotMovedMultiUse_		= (statsNotMovedMultiUse, 	\x s -> s { statsNotMovedMultiUse 	= x})
statsNotMovedDeepLambda_	= (statsNotMovedDeepLambda, 	\x s -> s { statsNotMovedDeepLambda 	= x})
statsNotMovedTopLevel_		= (statsNotMovedTopLevel, 	\x s -> s { statsNotMovedTopLevel 	= x})
statsNotMovedNoUses_		= (statsNotMovedNoUses, 	\x s -> s { statsNotMovedNoUses 	= x})
statsMissedUnboxing_		= (statsMissedUnboxing, 	\x s -> s { statsMissedUnboxing 	= x})
statsMoved_			= (statsMoved, 			\x s -> s { statsMoved		 	= x})
statsSharedForcings_		= (statsSharedForcings,		\x s -> s { statsSharedForcings	 	= x})
statsSharedUnboxings_		= (statsSharedUnboxings, 	\x s -> s { statsSharedUnboxings	= x})

	
instance Pretty Stats PMode where
 ppr ss
 	= "Float.Stats:\n"
	% "  * total bindings inspected                : " % statsTotalBindings ss			% "\n"
	% "  * number of bindings moved                : " % (length $ statsMoved ss)			% "\n" 
	% "  * number of bindings not moved because they..\n"
	% "      . were in the no-float set            : " % (length $ statsNotMovedNoFloat ss)		% "\n"
	% "      . had multiple uses                   : " % (length $ statsNotMovedMultiUse ss)	% "\n"
	% "      . were used at a deeper lambda level  : " % (length $ statsNotMovedDeepLambda ss)	% "\n"
	% "      . had top level effects               : " % (length $ statsNotMovedTopLevel ss)	% "\n"
	% "      . had no uses                         : " % (length $ statsNotMovedNoUses ss)		% "\n"
	% "  * number of bindings not moved, \n"
	% "       and all their uses were unboxings    : " % (length $ statsMissedUnboxing ss)		% "\n"
	% "  * number of shared forcings               : " % (length $ statsSharedForcings ss)		% "\n"
	% "  * number of shared unboxings              : " % (length $ statsSharedUnboxings ss)		% "\n"
	
