{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Base pretty printer support and fancy display options.
--   TODO: Stop using the pervasive PMode type. We should make pprStrPlain polymorphic
--         in the mode and just pass in the required mode string when we apply it.
module DDC.Main.Pretty
	( module DDC.Util.Pretty
	, PMode
	, Str
	, PrettyMode(..)
	, pprStrPlain )
where
import DDC.Util.Pretty


-- | The pretty printers for most data types take can look in PMode
--   to determine what options to use.
type PMode	
	= [PrettyMode]

-- | This is the type we usually use to represent printable text.
type Str = PrettyM PMode

-- | Pretty printing options that we support.
data PrettyMode
	-- | Annotate vars with their unique binds.
	= PrettyUnique

	-- | Show a '*' namespace qualified on tyoe variables.
	| PrettyTypeSpaces

	-- | Show the kinds of type variables and constructors.
	| PrettyTypeKinds

	-- | Show type annotations on variables in core.
	| PrettyCoreTypes

	-- | Show more-than constraints on effect variables in core.
	| PrettyCoreMore

	-- | Show type annotations on variables in the Sea language.
	| PrettySeaTypes
	deriving (Eq, Show)


-- | Pretty print something with no fancy options.
pprStrPlain :: Pretty a PMode => a -> String
pprStrPlain x
 	= pprStr [] x


