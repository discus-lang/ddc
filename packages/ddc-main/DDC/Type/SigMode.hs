
module DDC.Type.SigMode
	(SigMode(..))
where
import DDC.Main.Pretty

-- | How we should compare user provided type signatures
--   with inferred types.
data SigMode

	-- | Inferred sig must be less than given one, 
	--   with missing effects and closures taken to be:
	--     Top: for top level and stmt sigs
	--     Bot: for sigs in type class defs.
	= SigModeMatch

	-- | Inferred sig must match the given one exactly.
	| SigModeExact

	-- | Inferred sig must be less or equal the given one,
	--   with missing effects and closures taken to be Bot.
	| SigModeLess

	-- | Inferred sig must be more or equal the given one,
	--   with missing effects and closures taken to be Bot.
	| SigModeMore
	
	deriving (Show, Eq)


instance Pretty SigMode PMode where
 ppr mode 
  = case mode of
	SigModeMatch	-> ppr "::"
	SigModeExact	-> ppr "=:"
	SigModeLess	-> ppr "<:"
	SigModeMore	-> ppr ">:"
