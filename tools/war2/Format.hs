
-- | Constants and utils for formatting the test reports.
module Format 
	( formatPathWidth
	, formatTimeWidth
	, padR 
	, padL)
where


formatPathWidth :: Int
formatPathWidth	= 74

formatTimeWidth :: Int
formatTimeWidth = 7

padR n str
 	= str ++ replicate (n - length str) ' '  

padL n str
	= replicate (n - length str) ' '  ++ str
