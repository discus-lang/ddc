
module Shared.Base
	( SourcePos(..)
	, Literal  (..))
where

import Shared.Pretty
import Util

-----
data SourcePos		
	= SourcePos (String, Int, Int)
	deriving (Show, Eq)
	
instance Pretty SourcePos PMode where
 ppr (SourcePos (f, l, c))	= ppr $ f ++ ":" ++ show l ++ ":" ++ show (c - 1)


data Literal
	= LBool		Bool	-- for unboxed bools only
	| LInt		Int
	| LChar		Char
	| LFloat	Float
	| LString	String	
	deriving (Show, Eq)


instance Pretty Literal PMode where
 ppr lit 
  = case lit of
	LBool	b
	 -> case b of
	 	True	-> ppr "true"
		False	-> ppr "false"
		
  	LInt    i	-> ppr $ show i
	LChar   c	-> ppr $ show c
	LFloat  f	-> ppr $ show f
	LString s	-> ppr $ show s
