
-- | Combinators for forming new pretty things out of old ones.
module DDC.Util.Pretty.Combinators 
	( plain
	, blank
	, newline
	, paste,  (%)
	, punc,	  (%!%)
	, indent, (%>)
	, shift,  (%>>)
	, bump,   (<>)
	, vcat
	, vvcat
	, padRc, padR
	, padLc, padL 
	, parens
	, braces
	, ifMode
	, pprIf)
where
import DDC.Util.Pretty.Base

-- Operator precendences.
infixl 9 %!%
infixr 8 %>
infixr 7 %
infixr 7 <>


-- | Output some PrettyPrim thing with no fancy options.
plain :: PrettyPrim -> PrettyM m
plain x	= PrettyM (const x)

-- As the mode type 'm' is unconstrained in the thing above this breaks the
--	functional dependency coverage condition. (sadness)


-- | An empty pretty thing.
--	We need this because \"\" is the same as [], and we don't want to
--	print it as such. Sigh, we really want a different String type here.
blank :: PrettyM m
blank	= plain PNil


-- | A newline character.
newline :: PrettyM m
newline	= plain (PChar '\n')


-- Paste ----------------------------------------------------------------------
-- | Paste/append two pretty things together.
paste 	:: (Pretty a m, Pretty b m) => a -> b -> PrettyM m
paste a b 
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, fb m])


-- | Same as `paste`.
(%) 	:: (Pretty a m, Pretty b m) => a -> b -> PrettyM m
(%)	= paste
		

-- Punc -----------------------------------------------------------------------
-- | Punctuate a pretty thing between others.
punc	:: (Pretty a m, Pretty b m) => a -> [b] -> PrettyM m
punc p []		= blank
punc p (x : [])		= ppr x
punc p (x1 : x2 : xs)	= x1 % p % punc p (x2 : xs)


-- | Same as `punc`.
(%!%)	:: (Pretty a m, Pretty b m)
	=> a -> [b] -> PrettyM m
(%!%)	= punc


-- Indent ---------------------------------------------------------------------
-- | Indent a pretty thing to the right of another one.
--	Consecutive lines are also indented.
indent 	:: (Pretty a m, Pretty b m) => a -> b -> PrettyM m
indent a b 
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, PIndent $ fb m])


-- | Same as `indent`.
(%>) 	:: (Pretty a m, Pretty b m) => a -> b -> PrettyM m
(%>)  a b 
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, PIndent $ fb m])


-- Shift ---------------------------------------------------------------------- 
-- | Shift to the next tabstop.
--	Consecutive lines return to the original tabstop.
shift	:: (Pretty a m, Pretty b m) => a -> b -> PrettyM m
shift a b
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, PTabNext, fb m])


-- | Same as `shift`.
(%>>)	:: (Pretty a m, Pretty b m)
	=> a -> b -> PrettyM m
(%>>)	= shift


-- Bumping --------------------------------------------------------------------
-- | Put a space between these things.
bump	:: (Pretty [Char] m, Pretty a m, Pretty b m) => a -> b -> PrettyM m
bump a b = a % " " % b

-- | Same as bump.
(<>)	:: (Pretty [Char] m, Pretty a m, Pretty b m) => a -> b -> PrettyM m
(<>)	= bump


-- Concatenation --------------------------------------------------------------
-- | Same as (punc newline)
vcat	:: (Pretty [Char] m, Pretty a m) => [a] -> PrettyM m
vcat ps	= punc newline ps % newline


-- | Same as (punc (newline % newline))
vvcat	:: (Pretty [Char] m, Pretty a m) => [a] -> PrettyM m
vvcat ps = punc (newline % newline) ps % newline


-- Padding --------------------------------------------------------------------
-- | Pad into a right justified column.
padRc :: Pretty a m => Int -> Char -> a -> PrettyM m
padRc n c x
 = case ppr x of
  	PrettyM fx -> PrettyM (\m -> PPadRight n c (fx m))


-- | Pad something into a right justified column, with spaces.
padR :: Pretty a m => Int -> a -> PrettyM m
padR n x	= padRc n ' ' x

	
-- | Pad something into a left justified column.
padLc :: Pretty a m => Int -> Char -> a -> PrettyM m
padLc n c x
 = case ppr x of
 	PrettyM fx -> PrettyM (\m -> PPadLeft n c (fx m))


-- | Pad something into a left justified column, with spaces.
padL :: Pretty a m => Int -> a -> PrettyM m
padL n x	= padLc n ' ' x


-- Wrapping -------------------------------------------------------------------
-- | Wrap in parenthesis ().
parens :: (Pretty [Char] m, Pretty a m) => a -> PrettyM m
parens a	= "(" % a % ")"


-- | Wrap in braces {}.
braces :: (Pretty [Char] m, Pretty a m) => a -> PrettyM m
braces a	= "{" % a % "}"


-- IfMode ---------------------------------------------------------------------
-- | Choose a pretty thing based on the mode.
ifMode 
	:: forall a m
	.  Pretty a m
	=> (m -> Bool)	-- ^ Mode predicate.
	-> a		-- ^ Option to use if predicate is true.
	-> a		-- ^ Option to use if predicate is false.
	-> PrettyM m

ifMode fun x y
 = case (ppr x, ppr y) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> if fun m 
					then fa m
					else fb m)

-- | Print some thing if True, otherwise blank.
pprIf 	:: forall a m
	.  Pretty a m
	=> Bool -> a -> PrettyM m

pprIf b x
	| b		= ppr x
	| otherwise	= blank


