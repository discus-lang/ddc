-- | Pretty printer combinators
module Util.Pretty.Comb
	( Pretty  (..)
	, PrettyM (..)
	, pprStr

	, pNil

	, paste, (%)
	, punc,  (%!%)
	, padRc, padR
	, padLc, padL
	, ifMode

	, (%>)
	, (%>>)
	, (<>)
	, parens
	, braces)
where

-----
import Data.List
import Data.Maybe
import Data.Typeable

import Util.Misc
import Util.Generics

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import GHC.Exts

-- | The pretty printer class
--	A 'PrettyMode' data type should be defined by the caller
--	to manage the various ways it would want to print a structure.
--
class Pretty a mode | a -> mode where
 ppr		:: a -> PrettyM mode


-- | PrettyM holds a function which produces a pretty thing in a particular mode.
data PrettyM mode
	= PrettyM (mode -> PrettyP)


-- | PrettyP holds primitive things that can be printed on the screen, 
--	along with meta-commands for controlling the layout.
--	
data PrettyP
	= PNil				-- an empty pretty thing (no charaters)
	| PString String		-- a literal string
	| PChar   Char			-- a literal character

	| PAppend [PrettyP]		-- print all these things one after the other

	-- Lists have their own ctor so we can print Strings of [Char]
	-- without decoration, but other things as [ x1, x2, ... ]. 
	-- It would be better to use a different String type...
	| PList   [PrettyP]		

	-- | An indented pretty thing
	| PIndent PrettyP

	-- | A pretty thing in a left/right justified column of the given width
	--	with empty space padded with a char.
	| PPadLeft  Int Char PrettyP
	| PPadRight Int Char PrettyP
	
	-- | Indent commands
	| PTabAdd Int			-- change the indent level by some number of chars
	| PTabNext			-- move to the next tabstop

	deriving (Eq, Show)

-- didn't managed to get this to work...
-- instance IsString PrettyP where
-- fromString x	= PString x

-- | Render a pretty thing as a string.
pprStr 	:: Pretty a m
	=> m -> a -> String

pprStr mode x	
 = case ppr x of
 	PrettyM f -> render $ f mode


-- Primitive Combinators ---------------------------------------------------------------------------
infixr 7 %
infixr 8 %>
infixl 9 %!%

-- paste two pretty things together
paste 	:: (Pretty a m, Pretty b m)
	=> a -> b -> PrettyM m

paste a b 
 = case (ppr a, ppr b) of
 	(PrettyM fa, PrettyM fb)
		-> PrettyM (\m -> PAppend [fa m, fb m])

(%) 	:: (Pretty a m, Pretty b m)	
	=> a -> b -> PrettyM m
(%)	= paste
		
-- Intersperse a pretty thing between others
punc	:: (Pretty a m, Pretty b m)
	=> a -> [b] -> PrettyM m

punc p []		= pNil
punc p (x : [])		= ppr x
punc p (x1 : x2 : xs)	= x1 % p % punc p (x2 : xs)

(%!%)	:: (Pretty a m, Pretty b m)
	=> a -> [b] -> PrettyM m
(%!%)	= punc


-- Indent a pretty thing to the right of this one
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


-- Padding things out in left or right justified columns
padRc :: Pretty a m => Int -> Char -> a -> PrettyM m
padRc n c x
 = case ppr x of
  	PrettyM fx -> PrettyM (\m -> PPadRight n c (fx m))
	
padLc :: Pretty a m => Int -> Char -> a -> PrettyM m
padLc n c x
 = case ppr x of
 	PrettyM fx -> PrettyM (\m -> PPadLeft n c (fx m))


-- Choose a pretty thing based on the mode.
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


-- Derived Combinators -----------------------------------------------------------------------------
-- These are derived from more primitive combinators above.

infixr 7 <>

-- | An empty pretty thing.
--	We need this because "" is the same as [], and we don't want to print it as such.
--	sigh.. we really want a different String type here.
pNil		= plain PNil

-- | Put a space between these things.
(<>)  a b	= a % " " % b

-- | Wrap in parenthesis.
parens a	= "(" % a % ")"

-- | Wrap in brackets.
braces a	= "{" % a % "}"

-- | Pad into a right justified column.
padR n x	= padRc n ' ' x

-- | Pad into a left justified column.
padL n x	= padLc n ' ' x


-- Simple Instances  -------------------------------------------------------------------------------
-- These are instances for simple things that are always printed the same way,
-- independant of the printer mode.
-- As the mode type has no effect, so is left as tyvar, which breaks the
--	fundep coverage condition on the instances (yay!)

plain x	= PrettyM (\_ -> x)

-- self
instance Pretty (PrettyM m) m where
 ppr x	= x

-- base types
instance Pretty () m where
 ppr ()	= ppr "()"

instance Pretty Int m where
 ppr x	= plain $ PString $ show x

instance Pretty Char m where
 ppr x	= plain $ PChar x

instance Pretty Bool m where
 ppr x	= plain $ PString $ show x

instance Pretty Integer m where
 ppr x	= plain $ PString $ show x

instance Pretty Float m where
 ppr x 	= plain $ PString $ show x

instance Pretty Double m where
 ppr x 	= plain $ PString $ show x


-- maybe
instance Pretty a m => Pretty (Maybe a) m where
 ppr (Just x)	= "Just " % x
 ppr Nothing	= ppr "Nothing"

-- lists
instance Pretty a m => Pretty [a] m where
 ppr xs
 	= PrettyM (\m -> PList 	$ map (\x -> case ppr x of
						PrettyM fx	-> fx m)
				$ xs)
-- tuples
instance (Pretty a m, Pretty b m) 
	=> Pretty (a, b) m where
 ppr (a, b)	
 	= "(" % a % ", " % b % ")"

instance (Pretty a m, Pretty b m, Pretty c m) 
	=> Pretty (a, b, c) m where
 ppr (a, b, c)
 	= "(" % a % ", " % b % ", " % c % ")"

instance (Pretty a m, Pretty b m, Pretty c m, Pretty d m) 
	=> Pretty (a, b, c, d) m where
 ppr (a, b, c, d) 
 	= "(" % a % ", " % b % ", " % c % ", " % d % ")"

-- maps
instance (Pretty a m, Pretty b m) => Pretty (Map a b) m where
 ppr mm 
 	= braces
	$ punc ", " 
	$ map (\(x, y) -> x % " := " % y)
	$ Map.toList mm
 
-- sets
instance (Pretty a m) => Pretty (Set a) m where
 ppr ss	
 	= braces
	$ punc ", "
	$ Set.toList ss


-- Render ------------------------------------------------------------------------------------------

-- | the render state holds the current tab with and cursor position.
data RenderS
	= RenderS
	{ stateTabWidth		:: Int
	, stateIndent		:: Int
	, stateCol		:: Int }
	
initRenderS
	= RenderS
	{ stateTabWidth		= 8
	, stateIndent		= 0 
	, stateCol		= 0 }
	
-- render a pretty thing as a string	
render	:: PrettyP -> String
render xx	
	= render' initRenderS xx

render'	state xx
 	= spaceTab state $ reduce state xx

-- Reduce a PrettyP to a simpler list of commands in preparation for rendering.
reduce 	:: RenderS -> PrettyP -> [PrettyP]
reduce	state xx
 = case xx of
	PNil		-> [PNil]

 	PString	s	-> map PChar s

	PChar c		-> [PChar c]

	PList   zz@(PChar c:xs)	
	 -> zz

	PList 	zz	
	 -> [PChar '[']
	 ++ (concat $ intersperse (map PChar ", ") $ map (reduce state) zz) 
	 ++ [PChar ']']

	PAppend ss	-> concat $ map (reduce state) ss
	
	PIndent ss	
	 ->   [ PTabAdd (stateTabWidth state)]
	   ++ (reduce state) ss
	   ++ [PTabAdd (- (stateTabWidth state))]
	 
	PTabNext
	 -> [PTabNext]
	
	PPadLeft n c p	-> [PPadLeft n c  (PAppend $ reduce state p)]
	PPadRight n c p	-> [PPadRight n c (PAppend $ reduce state p)]
	

-- render a pretty thing as characters.	
spaceTab :: RenderS -> [PrettyP] -> [Char]
spaceTab state xx
 = case xx of
	PNil : xs	
	 ->    spaceTab state xs

 	PTabAdd i  :xs	
	 -> let state'	= state
	 		{ stateIndent	= (stateIndent state) + i }
	    in spaceTab state' xs
				

	PTabNext : xs
	 -> let tabHere	= (stateCol state) `div` (stateTabWidth state)
	 	tabNext	= (tabHere + 1) * (stateTabWidth state)
		pad	= replicate (tabNext - stateCol state) $ ' '

	 	state'	= state
	 		{ stateCol	= tabNext }

	   in	pad ++ spaceTab state' xs

	PChar '\n' : PTabAdd i : xs
	 -> spaceTab state (PTabAdd i : PChar '\n' : xs)

	PChar '\n' : xs	
	 -> let	pad	= replicate (stateIndent state) ' '
	 	state'	= state
	 		{ stateCol	= stateIndent state }

	    in  '\n' : (pad ++ spaceTab state' xs)
		
	PChar x : xs
	 -> let state'	= state
	 		{ stateCol	= stateCol state + 1 }
	    in  x : spaceTab state' xs

	PAppend xx : xs
	 -> spaceTab state (xx ++ xs)

	PPadLeft n c p : xs
	 -> let cs	= spaceTab state [p]
		csLen	= length cs
		colLen	= max csLen n
		state'	= state { stateCol = stateCol state + colLen }
	    in	cs ++ replicate (n - length cs) c ++ spaceTab state' xs
	    
	PPadRight n c p : xs
	 -> let cs	= spaceTab state [p]
		csLen	= length cs
		colLen	= max csLen n
		state'	= state { stateCol = stateCol state + colLen }
	    in	replicate (n - length cs) c ++ cs ++ spaceTab state' xs

	[]		-> []
		
	_		-> "Util.Pretty.Comb: no match for " ++ show xx

-- Test Code ---------------------------------------------------------------------------------------
{-
test1	
 = pprStr ()
 $ "perch " % "<" % ", " %!% ([1, 2, 3] :: [Int]) % ">\n" 
		%> "rabbit " %>> "frog\n" 
		%  "cat fish\n" 
			%> "walk " % "my" % "dog\n"
-}
