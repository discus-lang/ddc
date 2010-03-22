
-- | Base pretty printer support and fancy display options.
module DDC.Main.Pretty
	( module DDC.Util.Pretty
	, PMode
	, PrettyMode(..)
	, pprStrPlain )
where
import DDC.Util.Pretty


-- | The pretty printers for most data types take can look in PMode
--	to determine what options to use.
type PMode	
	= [PrettyMode]


-- | Pretty printing options that we support.
data PrettyMode
	= PrettyUnique		-- ^ Annotate vars with their uniqueBinds.
	| PrettyTypeSpaces	-- ^ Show a '*' namespace qualifier on type variables.
	| PrettyTypeKinds	-- ^ Show kinds on type vars and constructors.
	| PrettyCoreTypes	-- ^ Show type annotations on vars in core.
	| PrettyCoreMore	-- ^ Show more-than constraints on effect vars in core.
	deriving (Eq, Show)


-- | Pretty print something with no fancy options.
pprStrPlain :: Pretty a PMode => a -> String
pprStrPlain x
 	= pprStr [] x
