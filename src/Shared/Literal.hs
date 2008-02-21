
module Shared.Literal
	( Const (..)
	, Literal (..))
where

-----
import Shared.Base
import Shared.Pretty
import Util


-----
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
