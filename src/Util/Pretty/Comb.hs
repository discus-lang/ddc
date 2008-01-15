
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
	pNil,
	
	pprStr
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

import qualified Data.Set	as Set
import Data.Set			(Set)

-----------------------
class Pretty a where
 ppr		:: a -> PrettyP


pprStr x	= render $ ppr x

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
 ppr x		= x

instance Pretty Bool where
 ppr x		= PString $ show x
 
instance Pretty Int where
 ppr x 		= PString $ show x

instance Pretty Integer where
 ppr x		= PString $ show x

instance Pretty Float where
 ppr x		= PString $ show x
 
instance Pretty Char where
 ppr x		= PChar x


-----
-- Unit
--
instance Pretty () where
 ppr ()		= ppr "()"

-----
-- Lists
--
instance Pretty a => Pretty [a] where
 ppr xx		= PList $ map ppr xx


-----
-- Tuples
--
instance (Pretty a, Pretty b) 
	=> Pretty (a, b) where

 ppr (a, b)	
 	= "(" % a % ", " % b % ")"


instance (Pretty a, Pretty b, Pretty c) 
	=> Pretty (a, b, c) where

 ppr (a, b, c)
 	= "(" % a % ", " % b % ", " % c % ")"


instance (Pretty a, Pretty b, Pretty c, Pretty d) 
	=> Pretty (a, b, c, d) where

 ppr (a, b, c, d)
 	= "(" % a % ", " % b % ", " % c % ", " % d % ")"
	
-----
-- Maybe
--
instance Pretty a => Pretty (Maybe a) where
 ppr (Just x)	= "Just " % x
 ppr Nothing	= ppr "Nothing"


-----
-- Maps
--
instance (Pretty a, Pretty b) => Pretty (Map a b) where
 ppr m		= ppr $ Map.toList m
 

instance (Pretty a) => Pretty (Set a) where
 ppr ss	= "{" % ", " %!% Set.toList ss % "}"


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
(%)   a b	= PAppend [ppr a, ppr b]
(%>)  a b	= PAppend [ppr a, PIndent $ ppr b]
(%#>) a b	= PAppend [ppr a, PTabInc, ppr b]
(%#<) a b 	= PAppend [ppr a, PTabDec, ppr b]
(%>>) a b	= PAppend [ppr a, PTabNext, ppr b]
	
appendMapPretty	xx
	= PAppend $ map ppr xx
	
-----
(%!%)		:: (Pretty a, Pretty b)
		=> a -> [b] -> PrettyP
	
(%!%) i xx	= PAppend $ intersperse (ppr i) $ map ppr xx


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











