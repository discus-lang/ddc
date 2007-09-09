
module Shared.Bits
(
	annotMisc, scrubAnnotMisc,
	annotVar,  scrubAnnotVar,
	annotType, scrubAnnotType,
	scrubAnnots
)

where

-----
annotMisc ::	String	-> String
annotMisc	s	= "[`" ++ s ++ "]"

scrubAnnotMisc	s	= scrubAnnotC '`' s

-----
annotVar	s	= "[`" ++ s ++ "]"

scrubAnnotVar	s	= scrubAnnotC '`' s


-----
annotType	s	= "[`" ++ s ++ "]"

scrubAnnotType	s	= scrubAnnotC '`' s


-----
scrubAnnots	s	= scrubAnnotMisc $ scrubAnnotVar $ scrubAnnotType $ s



-----
scrubAnnotC ::	Char ->	String 	-> String
scrubAnnotC	c	[]	= []
scrubAnnotC	c	('[':x:xs)
 | c == x			= scrubAnnotR c xs
 | otherwise			= '[' : scrubAnnotC c (x:xs)

scrubAnnotC	c	(x:xs)	= x : scrubAnnotC c xs


scrubAnnotR ::	Char -> String -> String
scrubAnnotR	c	[]	= []
scrubAnnotR	c	xx
 = case xx of 
	(']':xs)	-> scrubAnnotC c xs
	(x:xs)		-> scrubAnnotR c xs
