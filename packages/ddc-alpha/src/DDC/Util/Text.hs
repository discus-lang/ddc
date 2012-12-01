
module DDC.Util.Text
	(normaliseSpaces)
where
import Data.Char

-- | Normalise a string so that runs of whitespace characters are 
--   converted to a single space.
normaliseSpaces :: String -> String
normaliseSpaces ss
 = case ss of
	[]	-> []
	c1 : []	-> [c1]

	c1 : c2 : rest
	 | isSpace c1
	 , isSpace c2
	 -> normaliseSpaces (' ' : rest)
	
 	 | otherwise
  	 -> c1 : normaliseSpaces (c2 : rest)
	
	