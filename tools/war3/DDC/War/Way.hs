
module DDC.War.Way
	( Way(..)
	, pprWayName)
where
	
-- | A way to build the test
--	This holds extra options to pass to the program.
data Way
	= WayNil
	| WayOpts 
		{ wayName	:: String 
		, wayOptsComp	:: [String] 
		, wayOptsRun	:: [String] }
	deriving (Eq, Ord, Show)

pprWayName :: Way -> String
pprWayName way
 = case way of
	WayNil		-> ""
	WayOpts{}	-> wayName way