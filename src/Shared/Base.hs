
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
 ppr (SourcePos (f, l, c))	= ppr $ f ++ ":" ++ show l ++ ":" ++ show (c - 1)
 ppr NoSourcePos		= ppr "@NoSourcePos"


data Literal
	= LInt		Int
	| LChar		Char
	| LFloat	Float
	| LString	String	
	deriving (Show, Eq)

instance Pretty Literal where
 ppr lit 
  = case lit of
  	LInt    i	-> ppr $ show i
	LChar   c	-> ppr $ show c
	LFloat  f	-> ppr $ show f
	LString s	-> ppr $ show s
