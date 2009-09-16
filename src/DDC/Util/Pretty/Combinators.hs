
-- | Combinators for forming new pretty things out of old ones.
module DDC.Util.Pretty.Combinators where

import DDC.Util.Pretty.Base


-- | As the mode type has no effect, so is left as tyvar, which breaks the
--	fundep coverage condition on the instances (yay!)
plain x	= PrettyM (const x)


-- | An empty pretty thing.
--	We need this because \"\" is the same as [], and we don't want to print it as such.
--	sigh.. we really want a different String type here.
pNil		= plain PNil


-- | paste/append two pretty things together
paste 	:: (Pretty a m, Pretty b m) 	
	=>  a -> b -> PrettyM m
paste a b 
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, fb m])

infixr 7 %

(%) 	:: (Pretty a m, Pretty b m)	
	=> a -> b -> PrettyM m
(%)	= paste
		

-- Intersperse a pretty thing between others
punc	:: (Pretty a m, Pretty b m)
	=> a -> [b] -> PrettyM m

punc p []		= pNil
punc p (x : [])		= ppr x
punc p (x1 : x2 : xs)	= x1 % p % punc p (x2 : xs)

infixl 9 %!%

(%!%)	:: (Pretty a m, Pretty b m)
	=> a -> [b] -> PrettyM m
(%!%)	= punc


-- Indent a pretty thing to the right of this one
infixr 8 %>

(%>) 	:: (Pretty a m, Pretty b m)
	=> a -> b -> PrettyM m

(%>)  a b 
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, PIndent $ fb m])


-- Move to the next tabstop
(%>>)	:: (Pretty a m, Pretty b m)
	=> a -> b -> PrettyM m
	
(%>>) a b
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, PTabNext, fb m])


-- | Padding things out in left or right justified columns
padRc :: Pretty a m => Int -> Char -> a -> PrettyM m
padRc n c x
 = case ppr x of
  	PrettyM fx -> PrettyM (\m -> PPadRight n c (fx m))
	
padLc :: Pretty a m => Int -> Char -> a -> PrettyM m
padLc n c x
 = case ppr x of
 	PrettyM fx -> PrettyM (\m -> PPadLeft n c (fx m))


-- | Choose a pretty thing based on the mode.
ifMode 
	:: Pretty a m
	=> (m -> Bool)	-- mode predicate
	-> a		-- option if predicate is true
	-> a		-- option if predicate is false
	-> PrettyM m

ifMode fun x y
 = case (ppr x, ppr y) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> if fun m 
					then fa m
					else fb m)


-- | Put a space between these things.
infixr 7 <>
(<>)  a b	= a % " " % b


-- | Wrap in parenthesis.
parens a	= "(" % a % ")"


-- | Wrap in brackets.
braces a	= "{" % a % "}"


-- | Pad into a right justified column.
padR n x	= padRc n ' ' x


-- | Pad into a left justified column.
padL n x	= padLc n ' ' x

