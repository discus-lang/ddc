{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Pretty printer library.
module DDC.Util.Pretty 
	( Pretty	(..)
	, StrMode
	, pprStr
	, module DDC.Util.Pretty.Render 
	, module DDC.Util.Pretty.Combinators
	, module DDC.Util.Pretty.Simple)
where
import DDC.Util.Pretty.Base
import DDC.Util.Pretty.Render
import DDC.Util.Pretty.Combinators
import DDC.Util.Pretty.Simple		()


-- | Render a pretty thing as a string.
pprStr 	:: Pretty a mode
	=> mode -> a -> String

pprStr mode x
	= renderWithMode mode $ ppr x
	
