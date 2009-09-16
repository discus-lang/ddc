
-- | Basic types for the pretty printer.
module DDC.Util.Pretty.Base 
	( Pretty 	(..)
	, PrettyM	(..)
	, PrettyPrim 	(..) )
where


-- | Class of things that can be pretty printed in a certain mode.
--	Client modules can decide what the possible modes are.
--
class Pretty a mode | a -> mode where
 	ppr	:: a -> PrettyM mode

-- | Holds a function that can produce some pretty things depending on 
--	what display mode is asked for.
data PrettyM mode
	= PrettyM (mode -> PrettyPrim)

-- | Pretty print myself.
instance Pretty (PrettyM m) m where
 	ppr x	= x


-- | PrettyPrim represents the text that can be printed to the screen,
--	in a reasonably efficient way.
data PrettyPrim
	= PNil				-- an empty pretty thing (no charaters)
	| PString String		-- a literal string
	| PChar   Char			-- a literal character

	| PAppend [PrettyPrim]		-- print all these things one after the other

	-- Lists have their own ctor so we can print Strings of [Char]
	-- without decoration, but other things as [ x1, x2, ... ]. 
	-- It would be better to use a different String type...
	| PList   [PrettyPrim]		

	-- | An indented pretty thing
	| PIndent PrettyPrim

	-- | A pretty thing in a left/right justified column of the given width
	--	with empty space padded with a char.
	| PPadLeft  Int Char PrettyPrim
	| PPadRight Int Char PrettyPrim
	
	-- | Indent commands
	| PTabAdd Int			-- change the indent level by some number of chars
	| PTabNext			-- move to the next tabstop
	deriving (Eq, Show)
