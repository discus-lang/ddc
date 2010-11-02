{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.Simplify.Stats
	( Stats(..)
	, statsProgress)
where
import DDC.Main.Pretty
import qualified Core.Float	as Float

-- | Statistics about what happened during a pass of the simplifier.
data Stats
	= Stats
	{ -- | Stats from the let-floating
	  statsFloat			:: Float.Stats

	  -- | number of unbox/box pairs eliminated
	, statsReducedUnboxBox		:: Int }


-- | Check the stats of a simplifier pass to see if we made any progress
statsProgress :: Stats -> Bool
statsProgress stats
	| statsReducedUnboxBox stats > 0				= True
	| not $ null $ Float.statsSharedForcings  $ statsFloat stats	= True
	| not $ null $ Float.statsSharedUnboxings $ statsFloat stats	= True
	| otherwise							= False


instance Pretty Stats PMode where
 ppr stats
 	= "Simplify.Stats\n"
	% "    - stats from let-floater:\n"
	%> (ppr (statsFloat stats))
	% "\n"
	% "    - unbox/boxings reduced   : " % (statsReducedUnboxBox stats) % "\n"



