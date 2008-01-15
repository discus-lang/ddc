
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
 ppr c 
  = case c of
  	CConst  literal	-> ppr literal
	CConstU literal	-> literal % "#"
