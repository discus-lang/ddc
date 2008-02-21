module Shared.Pretty
	( Pretty(..)
	, PrettyFlag(..)
	, PMode
	, pprStrPlain)
	
where

import Util

-- The pretty printing modes that we support
type PMode	= [PrettyFlag]

data PrettyFlag
	= PrettyUnique		-- annotate vars with their uniqueBinds
	deriving (Eq, Show)

pprStrPlain x
 	= pprStr [] x

