
module Util.Pretty.Comb
(
	Pretty (..),
	PrettyP,
	(%),
	(%>),
	(%#>),
	(%#<),
	(%!%),
	(%>>),

	appendMapPretty,

	reduce,
	initRenderS,
	pNil
)

where

-----
import Data.List
import Data.Maybe
import Data.Typeable

import Util.Misc
import Util.Generics

import qualified Data.Map	as Map
import Data.Map			(Map)

-----------------------
class Pretty a where
 pretty 	:: a -> String
 pretty  x	= render $ prettyp x

 prettyp	:: a -> PrettyP
 prettyp x	= PString $ pretty x


-----
data PrettyP
	= PNil	
	| PString String
	| PChar   Char
	| PList   [PrettyP]

	| PAppend [PrettyP]

	| PIndent PrettyP

	| PTabAdd Int

	| PTabInc 
	| PTabDec
	| PTabNext

	| PAnnot String		-- for debugging

	deriving (Eq, Data, Typeable, Show)

pNil	= PNil

-----
-- Base types
--
instance Pretty PrettyP where
 prettyp x	= x

instance Pretty Bool where
 prettyp x	= PString $ show x
 
instance Pretty Int where
 prettyp x 	= PString $ show x

instance Pretty Float where
 prettyp x	= PString $ show x
 
instance Pretty Char where
 prettyp x	= PChar x


-----
-- Unit
--
instance Pretty () where
 prettyp xx	= prettyp "()"

-----
-- Lists
--
instance Pretty a => Pretty [a] where
 prettyp xx	= PList $ map prettyp xx


-----
-- Tuples
--
instance (Pretty a, Pretty b) 
	=> Pretty (a, b) where

 prettyp (a, b)	
 	= "(" % a % ", " % b % ")"


instance (Pretty a, Pretty b, Pretty c) 
	=> Pretty (a, b, c) where

 prettyp (a, b, c)
 	= "(" % a % ", " % b % ", " % c % ")"


instance (Pretty a, Pretty b, Pretty c, Pretty d) 
	=> Pretty (a, b, c, d) where

 prettyp (a, b, c, d)
 	= "(" % a % ", " % b % ", " % c % ", " % d % ")"
	
-----
-- Maybe
--
instance Pretty a => Pretty (Maybe a) where
 prettyp (Just x)	= "Just " % x
 prettyp Nothing	= prettyp "Nothing"


-----
-- Maps
--
instance (Pretty a, Pretty b) => Pretty (Map a b) where
 prettyp m		= prettyp $ Map.toList m
 

-----------------------
-- render
---
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
	
	
render	:: PrettyP -> String
render xx	= render' initRenderS xx

render'	state xx
 = let ?state = state
   in   catMaybes
   	$ map toString 
	$ spaceTab state
	$ reduce xx


toString x
 = case x of
	PNil		-> Nothing
 	PChar c		-> Just c
	PAnnot _	-> Nothing
	
spaceTab :: RenderS -> [PrettyP] -> [PrettyP]
spaceTab state xx
 = case xx of
	PNil :xs	
	 ->    spaceTab state xs

 	PTabAdd i  :xs	
	 -> let state'	= state
	 		{ stateIndent	= (stateIndent state) + i }
	    in spaceTab state' xs
				

	PTabNext : xs
	 -> let tabHere	= (stateCol state) `div` (stateTabWidth state)
	 	tabNext	= (tabHere + 1) * (stateTabWidth state)
		pad	= replicate (tabNext - stateCol state) $ PChar ' '

	 	state'	= state
	 		{ stateCol	= tabNext }

		str	= "tabHere = " ++ show tabHere 
			++ " col  = " 	++ (show $ stateCol state)
			++ " next = " ++ show tabNext
			
	   in	PAnnot str : pad ++ spaceTab state' xs

	PChar '\n' : PTabAdd i : xs
	 -> spaceTab state (PTabAdd i : PChar '\n' : xs)

	PChar '\n' : xs	
	 -> let	pad	= replicate (stateIndent state) $ PChar ' '
	 	state'	= state
	 		{ stateCol	= stateIndent state }

	    in  PChar '\n' 
	    	:  pad 	
		++ spaceTab state' xs
			
	x:xs
	 -> let state'	= state
			{ stateCol	= (stateCol state) + 1 }
	    in x : spaceTab state' xs

	[]		-> []
	
	
reduce 	:: (?state :: RenderS)
	-> PrettyP -> [PrettyP]
reduce	xx
 = case xx of
	PNil	-> [PNil]

 	PString	s		
	 -> map PChar s

	PChar c
	 -> [PChar c]


	PList   zz@(PChar c:xs)	
	 -> zz

	PList 	zz	
	 -> [PChar '[']
	 ++ (concat $ intersperse (pcs ", ") $ map reduce zz) 
	 ++ [PChar ']']

	PAppend ss	
	 -> concat $ map reduce ss
	
	PIndent ss	
	 ->   [PTabAdd (stateTabWidth ?state)]
	   ++ reduce ss
	   ++ [PTabAdd (- (stateTabWidth ?state))]
	 
	PTabInc
	 -> [PTabAdd (stateTabWidth ?state)]
	 
	PTabDec
	 -> [PTabAdd (- (stateTabWidth ?state))]
	 
	PTabNext
	 -> [PTabNext]


pcs s	= map PChar s
	


-----------------------
-- combinators
--	
infixr 7 %, %#>, %#<
infixr 8 %>
infixl 9 %!%

type PComb2
	= (Pretty a, Pretty b)
	=> a -> b -> PrettyP


-----
(%) 		:: PComb2
(%)   a b	= PAppend [prettyp a, prettyp b]
(%>)  a b	= PAppend [prettyp a, PIndent $ prettyp b]
(%#>) a b	= PAppend [prettyp a, PTabInc, prettyp b]
(%#<) a b 	= PAppend [prettyp a, PTabDec, prettyp b]
(%>>) a b	= PAppend [prettyp a, PTabNext, prettyp b]
	
appendMapPretty	xx
	= PAppend $ map prettyp xx
	
-----
(%!%)		:: (Pretty a, Pretty b)
		=> a -> [b] -> PrettyP
	
(%!%) i xx	= PAppend $ intersperse (prettyp i) $ map prettyp xx


------
test1	
 = let	?state	= initRenderS
   in   spaceTab ?state
   	$ reduce
	$ "fish " % "<" % ", " %!% ([1, 2, 3] :: [Int]) % ">\n" 
		%> "dude " %>> "that\n" 
		%  "bites fish\n" 
			%> "walk " % "my" % "dog\n"


test1' 
	= putStr
	$ catMaybes
	$ map toString
	$ test1











