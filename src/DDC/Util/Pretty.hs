
-- | Pretty printer library.
module DDC.Util.Pretty 
	( Pretty	(..)
	, PrettyM
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
pprStr 	:: Pretty a m
	=> m -> a -> String

pprStr mode x	
 = case ppr x of
 	PrettyM f -> render $ f mode
