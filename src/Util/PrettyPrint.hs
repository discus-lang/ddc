
module Util.PrettyPrint
(
	indent,
	tabSet,
	tabGet,
	tabGetA,
	tabAdvance,
	tabSC,
	tabRs,
	tabR, tabRn, tabRnA,
	tabL, tabLn,
	tabReset,
	tabPush,
	tabPop,
	tabPushSC,

	State(..),
	initState,
	spaceOut,
)

where

-----
import Util.List
import Util.Math

-----
indent  ss
	| []	<- ss
	= ""

 	| last ss == '\n'
 	= tabR ++ tabAdvance ++ init ss ++ tabL ++ "\n"
 
	| otherwise
	= tabR ++ tabAdvance ++ ss ++ tabL 



tabSet name		= "\27#!{" ++ name ++ "}"
tabGet name		= "\27#${" ++ name ++ "}"

tabAdvance		= "\27#>"

tabR			= "\27#++"
tabRn  (count :: Int)	= "\27#+{" ++ show count ++ "}"

tabL			= "\27#--"
tabLn  (count :: Int)	= "\27#-{" ++ show count ++ "}"

tabSC			= "\27#sc"
tabReset		= "\27#r"

tabPush			= "\27#p"
tabPop			= "\27#o"


tabRs  count		= tabRn count ++ tabAdvance ++ tabLn count
tabGetA name		= tabGet name ++ tabAdvance

tabPushSC 		= tabPush ++ tabSC

tabRnA (count :: Int)	= tabRn count ++ tabAdvance


-----
-- State
--
data State
	= State {
		tabWidth	:: Int,			-- width of tabs

		indentTable	:: [(String, Int)],	-- table of stored indentations

		column		:: Int,			-- current column
		tabStop		:: Int,			-- current indentation

		sourceTabWidth	:: Int,
		stack		:: [Int]
	}

initState :: State
initState
	= State 
	{ tabWidth 		= 8
	, tabStop		= 0
	, column		= 0
	, indentTable		= [] 
	, sourceTabWidth	= 8
	, stack			= []
	}


-----
-- spaceOut
--
spaceOut ::	String ->	String
spaceOut    	xx		= spaceOutR initState xx

	
spaceOutR :: 	State ->	String -> String
spaceOutR	state		str
 = case str of
	-- store current column as named tabStop
	('\27':'#':'!':'{':xs)	->
	 let	(nameS, rest)	= splitOn '}' xs
		name		= init nameS
	 	state'		= state { indentTable	= (name, column state) : (indentTable state) }
	 in 	
	 	spaceOutR state' rest

	 
	-- load named tabStop into current tabStop
	('\27':'#':'$':'{':xs)	->
	 let 	(nameS, rest)	= splitOn '}' xs
		name		= init nameS
	 in 	
	 	case lookup name (indentTable state) of
	  	 Nothing	-> "\27#${" ++ spaceOutR state xs
	  	 Just n		-> spaceOutR state { tabStop = n } rest


	-- advance to current tabStop, if already passed tabStop then do nothing.
	('\27':'#':'>':xs)	-> 
	 let	count		= clamp0 $ tabStop state - column state
	 	pad		= replicate count ' '
		state'		= state { column	= tabStop state}
	 in
	 	pad ++ spaceOutR state' xs
		
	 
	-- shift tabStop right by specified amount
	('\27':'#':'+':'{':xs) -> 
	 let	(addS', rest)	= splitOn '}' xs
		addS		= init addS'
	 in 
	 	case mread addS of
		 Just add	-> spaceOutR state { tabStop = tabStop state + add } rest
		 Nothing	-> "\27#+{" ++ spaceOutR state xs
		 

	-- shift tabStop left by specificed amount
	('\27':'#':'-':'{':xs)	->
	 let	(addS', rest)	= splitOn '}' xs
		addS		= init addS'
	 in
	 	case mread addS of
		 Just add	-> spaceOutR state { tabStop = clamp0 $ tabStop state - add } rest
		 Nothing	-> "\27#-{" ++ spaceOutR state xs	
	
		 
	-- shift tabStop right by default tabWidth
	('\27':'#':'+':'+':xs)	->
	 let	state'		= state { tabStop	= tabStop state + tabWidth state }
	 in	spaceOutR state' xs
	 

	-- shift tabStop left by default tabWidth
	('\27':'#':'-':'-':xs)	->
	 let	state'		= state { tabStop	= clamp0 $ tabStop state - tabWidth state }
	 in	spaceOutR state' xs

	-- set tabStop to current collumn
	('\27':'#':'s':'c':xs)	-> 
	 let	state'		= state { tabStop	= column state }
	 in	spaceOutR state' xs

	-- reset tabStop to zero
	('\27':'#':'r':xs)	-> 
	 let	state'		= state { tabStop	= 0 }
	 in	spaceOutR state' xs

	-- push current tabStop onto stack
	('\27':'#':'p':xs)	->
	 let 	state'		= state	{ stack		= (tabStop state) : (stack state) }
	 in	spaceOutR state' xs
	 
	-- pop current tabStop from stack
	('\27':'#':'o':xs)	->
   	 if 	stack state	== [] 
	  then	spaceOutR state xs
	  else let
	  	state'		= state { stack		= tail (stack state)
					, tabStop	= head (stack state) }
					
		in spaceOutR state' xs
		
	-- a newline, pass the newline through, then advance back to current tabStop.
	('\n':xs)		-> 
	 let	state'	= state { column = tabStop state }
	    	pad	= replicate (tabStop state) ' '
	 in 
	 	"\n" ++ pad ++ spaceOutR state' xs

	
	-- empty list, all done
	[]			-> []

	-- just a regular character.
	(x:xs)		-> 
 	 let	state'	= state { column = column state + 1 }
	 in	x : spaceOutR state' xs













