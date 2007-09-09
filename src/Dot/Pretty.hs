
module Dot.Pretty
(
	

)

where

import Util
import qualified Shared.Var	as Var
import Shared.Var		(Var)

import Dot.Exp


instance Pretty Graph where
 prettyp gg
  = case gg of
  	Graph ss	
	 -> "graph foo {\n"
	 	%> ("\n" %!% ss)
	 %  "\n}\n"
	 
	DiGraph ss
	 -> "digraph foo {\n"
	 	%> ("\n" %!% ss)
	 %  "\n}\n"
	 
	 
instance Pretty Stmt where
 prettyp ss
  = case ss of
  	SEdge n1 n2	-> n1 % " -> " % n2
	SNode n1 aa	-> n1 % aa

	
instance Pretty NodeId where
 prettyp ss
  = case ss of
  	NVar v		-> prettyp $ show $ pretty (Var.bind v)
	NString s	-> prettyp $ show s


instance Pretty Attr where
 prettyp aa
  = case aa of
  	ALabel s	-> "label = " % show s
	AColor s	-> "color = " % show s
	
