
-- | Make a usage help page from this list of options.
module Util.Options.Help
	( makeOptionHelp )
where
import Util.Options.Option
import Util.Pretty


-- | Make a help page from this list of options
makeOptionHelp 
	:: Int 			-- indent level of descriptions
	-> [String] 		-- tags of the sections to show
	-> [Option a] 		-- options
	-> String

makeOptionHelp indent secs os
	= concat 
	$ (if elem "contents" secs
		then "\n  -- Contents --\n"
		else "")
	:  makeOptionHelp' indent secs False os

makeOptionHelp' _ _ _ []	
	= []
	
makeOptionHelp' indent secs squash (o:os)
 = case o of
	OGroup tag name
	 -- print a contents entry, but not the body of the section
	 | elem "contents" secs
	 -> "  " : makeHelp indent o : makeOptionHelp' indent secs True os

	 -- print an interesting section
	 | elem tag secs || elem "all" secs
	 -> "\n  " : makeHelp indent o : makeOptionHelp' indent secs False os
		
	 -- skip over a boring section
	 | otherwise
	 -> makeOptionHelp' indent secs True os
		
	_ 
		| squash	
		-> makeOptionHelp' indent secs squash os
		
		| otherwise	
		-> makeHelp indent o
		:  makeOptionHelp' indent secs squash os
			

makeHelp indent o
 = case o of
	ODefault optF
	 -> ""

 	OGroup tag name	
	 -> name ++ " (" ++ tag ++ ")\n"

	OFlag _ names desc
	 -> pprStr ()
	 $  padL indent ("    " % punc ", " names) % desc % "\n"

	OOpt  _ names use desc
	 -> pprStr ()
	 $  padL indent ("    " % use) % desc % "\n"

	OOpts  _ names use desc
	 -> pprStr ()
	 $  padL indent ("    " % use) % desc % "\n"
	 
	OOptEscape _ names use desc
	 -> pprStr ()
	 $  padL indent ("    " % use) % desc % "\n"
	
	OBlank
	 -> "\n"
