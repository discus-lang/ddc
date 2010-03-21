
-- | Base pretty printer support and fancy display options.
module DDC.Main.Pretty
	( module DDC.Util.Pretty
	, PMode
	, PrettyMode(..)
	, pprStrPlain )
where
import DDC.Util.Pretty


-- The pretty printing modes that we support
type PMode	
	= [PrettyMode]


-- | Pretty printing options that we support.
data PrettyMode
	= PrettyUnique		-- ^ annotate vars with their uniqueBinds
	| PrettyTypeSpaces	-- ^ show a '*' namespace qualifier on type variables.
	| PrettyTypeKinds	-- ^ show kinds on type vars and constructors
	| PrettyCoreTypes	-- ^ show type annotations on vars in core
	| PrettyCoreMore	-- ^ show :> constraints on type vars in core
	deriving (Eq, Show)


-- | Pretty print something with no fancy options.
pprStrPlain x
 	= pprStr [] x
