{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Core.Float.Stats where
import DDC.Var
import DDC.Main.Pretty


-- | Status about how many bindings were moved / notmoved
--   These are lists instead of sets, to reduce the overhead of collecting up the stats.
data Stats
	= Stats
	{ -- | Total number of bindings inspected
	  statsTotalBindings	:: Int
	
	  -- | Bindings that were not moved because they were in the no-float table
	, statsNotMovedNoFloat	:: [Var]

	  -- | Bindings that were not moved because they had multiple uses
	, statsNotMovedMultiUse	:: [Var]

	  -- | Bindings that were not moved because their use was at a deeper lambda level
	, statsNotMovedDeepLambda :: [Var]
	
	  -- | Bindings that were not moved because they had unmaskable effects.
	, statsNotMovedEffects	:: [Var]
	
	  -- | Bindings that were not moved because they had no uses.
	, statsNotMovedNoUses	:: [Var]
		
	  -- | Bindings that were not moved, and all their uses were directly under unboxings
	  -- .. not all of these are total failures, because the stmt might not have boxed the value itself.
	, statsMissedUnboxing	:: [Var]
		
	  -- | Bindings that were successfully moved
	, statsMoved		:: [Var] 

	  -- | Calls to force successfully shared
	, statsSharedForcings	:: [Var]

	  -- | Unboxings successfully shared
	, statsSharedUnboxings	:: [Var] }
	deriving Show

	
-- empty stats table
statsZero
	= Stats
	{ statsTotalBindings		= 0
	, statsNotMovedNoFloat		= []
	, statsNotMovedMultiUse		= []
	, statsNotMovedDeepLambda	= []
	, statsNotMovedEffects		= []
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
statsNotMovedEffects_		= (statsNotMovedEffects, 	\x s -> s { statsNotMovedEffects 	= x})
statsNotMovedNoUses_		= (statsNotMovedNoUses, 	\x s -> s { statsNotMovedNoUses 	= x})
statsMissedUnboxing_		= (statsMissedUnboxing, 	\x s -> s { statsMissedUnboxing 	= x})
statsMoved_			= (statsMoved, 			\x s -> s { statsMoved		 	= x})
statsSharedForcings_		= (statsSharedForcings,		\x s -> s { statsSharedForcings	 	= x})
statsSharedUnboxings_		= (statsSharedUnboxings, 	\x s -> s { statsSharedUnboxings	= x})

	
instance Pretty Stats PMode where
 ppr ss
 	= vcat
	[ ppr "Float.Stats:"
	, ppr "  - total bindings inspected                : " % statsTotalBindings ss
	, ppr "  - number of bindings moved                : " % (length $ statsMoved ss)
	, ppr "  - number of bindings not moved because they.."
	, ppr "      . were in the no-float set            : " % (length $ statsNotMovedNoFloat ss)
	, ppr "      . had multiple uses                   : " % (length $ statsNotMovedMultiUse ss)
	, ppr "      . were used at a deeper lambda level  : " % (length $ statsNotMovedDeepLambda ss)
	, ppr "      . had unmaskable effects              : " % (length $ statsNotMovedEffects ss)
	, ppr "      . had no uses                         : " % (length $ statsNotMovedNoUses ss)
	, ppr "  - number of bindings not moved,"
	, ppr "       and all their uses were unboxings    : " % (length $ statsMissedUnboxing ss)
	, ppr "  - number of shared forcings               : " % (length $ statsSharedForcings ss)
	, ppr "  - number of shared unboxings              : " % (length $ statsSharedUnboxings ss) ]
	
