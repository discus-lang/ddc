
module Shared.Literal
	( Literal    (..)
	, LiteralFmt (..))
where

import Shared.Base
import Shared.Error
import Shared.Pretty
import Util

-----
stage	= "Shared.Literal"

-- | A literal value	
data Literal
	= LBool		Bool
	| LWord		Integer
	| LInt		Integer
	| LFloat	Double
	| LChar		Char
	| LString	String	
	deriving (Show, Eq)

-- | A Literal value with an embeded format specifier
data LiteralFmt
	= LiteralFmt	Literal DataFormat
	deriving (Show, Eq)
	
	
instance Pretty LiteralFmt PMode where
 ppr (LiteralFmt lit fmt)
  = case (fmt, lit) of
	(Unboxed,	LBool True)	-> ppr "true#"
	(Unboxed, 	LBool False)	-> ppr "false#"
	
	(Boxed,   	LWord w)	-> w % "u"
	(BoxedBits b,	LWord w)	-> w % "u"  % b
	(Unboxed,  	LWord w)	-> w % "#u"
	(UnboxedBits b,	LWord w)	-> w % "#u" % b

	(Boxed,   	LInt i)		-> ppr i
	(BoxedBits b,	LInt i)		-> i % "i"  % b
	(Unboxed,  	LInt i)		-> i % "#i"
	(UnboxedBits b,	LInt i)		-> i % "#i" % b

	(Boxed,   	LFloat f)	-> ppr f
	(BoxedBits b,	LFloat f)	-> f % "f"  % show b
	(Unboxed,  	LFloat f)	-> f % "#f"
	(UnboxedBits b,	LFloat f)	-> f % "#i" % show b
	
	(Boxed, 	LChar c)        -> ppr $ show c
	(BoxedBits b,	LChar c)        -> c % "c" % show b
	(Unboxed,       LChar c)        -> ppr $ show c % "#"
	(UnboxedBits b,	LChar c)        -> show c % "#"

	(Boxed, 	LString s)      -> ppr $ show s
	(Unboxed,	LString s)      -> show s % "#"
	
	(_, _)	-> panic stage
		$  "showFormatLiteral: bad combination of format and literal value\n"
		%  "    format  = " % show fmt % "\n"
		%  "    literal = " % show lit % "\n" 


instance Pretty Literal PMode where
 ppr lit 
  = case lit of
	-- unboxed literals
	LBool	b
	 -> case b of
	 	True	-> ppr "true"
		False	-> ppr "false"
		
	LWord i		-> ppr $ show i 		
  	LInt    i	-> ppr $ show i
	LFloat  f	-> ppr $ show f
	LChar   c	-> ppr $ show c
	LString s	-> ppr $ show s


-----
{-
data Const
	-- boxed constants
	= CConst	Literal		

	-- unboxed constants
	| CConstU	Literal	
	deriving (Show, Eq)

-----
instance Pretty Const PMode where
 ppr c 
  = case c of
  	CConst  literal	-> ppr literal
	CConstU literal	-> literal % "#"
-}
