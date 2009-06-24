{-# OPTIONS -O2 #-}

module Shared.Vanilla
(
	vanillaName,
	vanillaChar,
	symbolNames
)

where

-----
import Util.Maybe

-----
-- vanillaName
--	Rewrites symbol charaters in a string to their alphanumeric names.
--
vanillaName ::	String	-> String
vanillaName	ss
	= concat $ map vanillaChar ss

vanillaChar ::	Char		-> String
vanillaChar	c	
	= fromMaybe [c] (lookup c symbolNames)


symbolNames ::	[(Char, String)]
symbolNames 	= 
	[ ('\\', "Bs")
	, ('=',  "Eq")
	, (':',  "Col")
	, ('|',  "Bar")
	, ('!',  "Bang")
	, ('#',  "Hash")
	, ('$',  "Dol")
	, ('%',  "Per")
	, ('&',  "Amp")
	, ('*',  "Star")
	, ('+',  "Plus")
	, ('.',  "Dot")
	, ('/',  "Fs")
	, ('<',  "Lt")
	, ('>',  "Gt")
	, ('?',  "Ques")
	, ('@',  "At")
	, ('^',  "Hat")
	, ('-',  "Dash")
	, ('~',  "Tild")
	, ('\'', "Dash") ]


	

