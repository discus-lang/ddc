
-- | Base types for the pretty printer library
module DDC.Util.Pretty 
	( Pretty	(..)
	, PrettyM
	, module DDC.Util.Pretty.Render 
	, module DDC.Util.Pretty.Combinators 	

	, pprStr)
where

import	DDC.Util.Pretty.Base
import	DDC.Util.Pretty.Render
import	DDC.Util.Pretty.Simple
import	DDC.Util.Pretty.Combinators


-- | Render a pretty thing as a string.
pprStr 	:: Pretty a m
	=> m -> a -> String

pprStr mode x	
 = case ppr x of
 	PrettyM f -> render $ f mode
