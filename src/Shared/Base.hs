
module Shared.Base
(
	SourcePos(..),
	Literal  (..)
)

where

import Util

-----
data SourcePos		
	= SourcePos (String, Int, Int)
	| NoSourcePos
	deriving (Show, Eq)
	
instance Pretty SourcePos where
 pretty (SourcePos (f, l, c))	= f ++ ":" ++ show l ++ ":" ++ show c
 pretty NoSourcePos		= "@NoSourcePos"


data Literal
	= LInt		Int
	| LChar		Char
	| LFloat	Float
	| LString	String	
	deriving (Show, Eq)

instance Pretty Literal where
 pretty lit 
  = case lit of
  	LInt    i	-> show i
	LChar   c	-> show c
	LFloat  f	-> show f
	LString s	-> show s
