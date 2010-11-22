{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- Only the parts that have a different presentation depending on the mode
-- should be represented as functions. Make the renderer carry the mode through
-- and apply it to the configurable parts. Otherwise append should be fast.
-- Store Data.Text at the nodes of an app-tree, and use test builder to make it.

-- | Combinators for forming new pretty things out of old ones.
module DDC.Util.Pretty.Combinators 
	( blank
	, newline, nl, nlnl
	, paste,  (%), (%%)
	, punc,	  (%!%)
	, indent, (%>)
	, shift,  (%>>)
	, vcat
	, vvcat
	, padRc,  padR
	, padLc,  padL 
	, parens
	, braces

	, brackets
	, pprIfMode
	, pprWhen
	, pprAtColumn)
where
import DDC.Util.Pretty.Base

-- Operator precendences.
-- infixl 9 %!%
infixr 8 %>
infixr 7 %
infixr 7 %%

-- | An empty pretty thing.
blank :: StrMode mode
blank	= PBlank


-- | A newline character.
newline :: StrMode mode
newline	= PNewLine

nl	:: StrMode mode
nl	= PNewLine

nlnl	:: StrMode mode
nlnl	= PAppend [PNewLine, PNewLine]

-- Paste ----------------------------------------------------------------------
-- | Paste/append two pretty things together.
paste 	:: (Pretty a mode, Pretty b mode)
	=> a -> b -> StrMode mode

paste a b 
	= PAppend [ppr a, ppr b]


-- | Same as `paste`.
(%) 	:: (Pretty a mode, Pretty b mode) 
	=> a -> b -> StrMode mode

(%)	= paste

		
-- | Paste with a space
(%%)	:: (Pretty a mode, Pretty b mode)
	=> a -> b -> StrMode mode

(%%) x y = x % PString " " % y


-- Punc -----------------------------------------------------------------------
-- | Punctuate some pretty things with another.
punc	:: (Pretty a mode, Pretty b mode)
	=> a -> [b] -> StrMode mode

punc _ []		= blank
punc _ (x : [])		= ppr x
punc p (x1 : x2 : xs)	= x1 % p % punc p (x2 : xs)


-- | Same as `punc`.
(%!%)	:: (Pretty a mode, Pretty b mode)
	=> a -> [b] -> StrMode mode
	
(%!%)	= punc


-- Indent ---------------------------------------------------------------------
-- | Indent a pretty thing, consecutive lines are also indented.
indent 	:: Pretty a mode => a -> StrMode mode
indent a = PIndent (ppr a)
	
-- | Indent a thing to the right of another.
(%>) 	:: (Pretty a mode, Pretty b mode) 
	=> a -> b -> StrMode mode

(%>) a b = PAppend [ppr a, PChar ' ', indent b]


-- Next ---------------------------------------------------------------------- 
-- | Shift to the next tabstop.
--	Consecutive lines return to the original tabstop.
shift	:: Pretty a mode
	=> a -> StrMode mode

shift a	= PShift (ppr a)


-- | Shift a thing to the next tabstop after the end of another.
(%>>)	:: (Pretty a mode, Pretty b mode)
	=> a -> b -> StrMode mode

(%>>) a b = PAppend [ppr a, PChar ' ', shift b]



-- Concatenation --------------------------------------------------------------
-- | Same as (punc newline)
vcat	:: (Pretty [Char] mode, Pretty a mode) 
	=> [a] -> StrMode mode

vcat ps	= punc newline ps % newline


-- | Same as (punc (newline % newline))
vvcat	:: (Pretty [Char] mode, Pretty a mode)
	=> [a] -> StrMode mode

vvcat ps = punc (newline % newline) ps % newline


-- Padding --------------------------------------------------------------------
-- | Pad into a right justified column.
padRc :: Pretty a mode => Int -> Char -> a -> StrMode mode
padRc n c x	= PPadRight n c (ppr x)

-- | Pad something into a right justified column, with spaces.
padR :: Pretty a mode => Int -> a -> StrMode mode
padR n x	= padRc n ' ' x

	
-- | Pad something into a left justified column.
padLc :: Pretty a mode => Int -> Char -> a -> StrMode mode
padLc n c x	= PPadLeft n c (ppr x)

-- | Pad something into a left justified column, with spaces.
padL :: Pretty a mode => Int -> a -> StrMode mode
padL n x	= padLc n ' ' x


-- Wrapping -------------------------------------------------------------------
-- | Wrap in parenthesis ().
parens 	:: (Pretty [Char] mode, Pretty a mode) 
	=> a -> StrMode mode
parens a	= "(" % a % ")"


-- | Wrap in braces {}.
braces 	:: (Pretty [Char] mode, Pretty a mode)
	=> a -> StrMode mode
braces a	= "{" % a % "}"

-- | Braces block
-- @ 
--   {    line1
--        line2 }
-- @    
{-
bblock :: (Pretty [Char] mode, Pretty a mode)

bblock 	ss
	= "{" % nl %> vcat ss %% "}" % nl
-}
-- | Wrap in brackets []
brackets :: (Pretty [Char] mode, Pretty a mode)
 	=> a -> StrMode mode
brackets a	= "[" % a % "]"


-- | Decide what to print based on the mode
pprIfMode
	:: forall a mode
	.  Pretty a mode
	=> (mode -> Bool)	-- ^ Mode predicate.
	-> a			-- ^ Thing to print if predicate is true.
	-> a			-- ^ Thing to print otherwise.
	-> StrMode mode

pprIfMode fun x y
	= PModal 
	$ \mode	-> if fun mode 
			then ppr x
			else ppr y


-- | Print some thing if True, otherwise blank.
pprWhen :: forall a mode
	.  Pretty a mode
	=> Bool -> a -> StrMode mode

pprWhen b x
	| b		= ppr x
	| otherwise	= blank


-- | Set the starting column for pretty printing
pprAtColumn :: Pretty a mode => Int -> a -> StrMode mode
pprAtColumn col str
	= PSetColumn col (ppr str)
