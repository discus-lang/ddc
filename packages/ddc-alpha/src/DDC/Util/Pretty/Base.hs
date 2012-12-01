{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# LANGUAGE 	MultiParamTypeClasses, FunctionalDependencies,
		FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}

-- | Basic types for the pretty printer.
module DDC.Util.Pretty.Base 
	( Pretty 	(..)
	, StrMode	(..))
where
import Data.Text.Lazy	(Text)

-- | Class of things that can be pretty printed in a certain mode.
--  Client modules can decide what the possible modes are.
class Pretty a mode | a -> mode where
 	ppr	:: a -> StrMode mode

-- | Pretty print myself.
instance Pretty (StrMode m) m where
 	ppr x	= x

instance Pretty Char m where
 ppr x	= PChar x

-- | PrettyPrim represents the text that can be printed to the screen,
--	in a reasonably efficient way.
data StrMode mode
	-- | A blank thing, no characters.
	= PBlank

	-- | A literal string, this shouldn't contain newlines.
	| PString String

	-- | Some literal text, this shouldn't contain newlines
	| PText  Text

	-- | A literal character.
	| PChar   Char

	-- | A raw newline character.
	| PNewLine

	-- | A thing with a different representation depending on the mode.
	| PModal (mode -> StrMode mode)

	-- | Print some things one after another.
	| PAppend [StrMode mode]

	-- | Move to the next tabstop.
	--   Consecutive lines return to the previous stop.
	| PShift (StrMode mode)

	-- | An indented pretty thing.
	| PIndent (StrMode mode)

	-- | Set the starting column.
	| PSetColumn Int (StrMode mode)

	-- | A pretty thing in a left/right justified column of the given width
	--   with empty space padded with a char.
	| PPadLeft  Int Char (StrMode mode)
	| PPadRight Int Char (StrMode mode)


