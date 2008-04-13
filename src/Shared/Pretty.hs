module Shared.Pretty
	( Pretty(..)
	, PrettyMode(..)
	, PMode
	, pprStrPlain)
	
where

import Util

-- The pretty printing modes that we support
type PMode	= [PrettyMode]

data PrettyMode
	= PrettyUnique		-- annotate vars with their uniqueBinds
	| PrettyTypeSpaces	-- show a '*' namespace qualifier on type variables.
	| PrettyTypeKinds	-- show kinds on type vars and constructors
	| PrettyCoreTypes	-- show type annotations on vars in core
	deriving (Eq, Show)

pprStrPlain x
 	= pprStr [] x


