
module Shared.Literal
	( Const (..)
	, Literal (..))
where

-----
import Util

-----
import Shared.Pretty
import Shared.Base

-----
data Const
	-- boxed constants
	= CConst	Literal		

	-- unboxed constants
	| CConstU	Literal	
	deriving (Show, Eq)

-----
instance Pretty Const where
 prettyp c 
  = case c of
  	CConst  literal	-> prettyp literal
	CConstU literal	-> literal % "#"
