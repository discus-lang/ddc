
module Dot.Pretty
	()
where
import Dot.Exp
import qualified Shared.Var	as Var
import DDC.Main.Pretty

instance Pretty Graph PMode where
 ppr gg
  = case gg of
  	Graph ss	
	 -> "graph foo {\n"
	 	%> ("\n" %!% ss)
	 %  "\n}\n"
	 
	DiGraph ss
	 -> "digraph foo {\n"
	 	%> ("\n" %!% ss)
	 %  "\n}\n"
	 
	 
instance Pretty Stmt PMode where
 ppr ss
  = case ss of
  	SEdge n1 n2	-> n1 % " -> " % n2
	SNode n1 aa	-> n1 % aa

	
instance Pretty NodeId PMode where
 ppr ss
  = case ss of
  	NVar v		-> ppr $ (Var.varId v)
	NString s	-> ppr $ show s


instance Pretty Attr PMode where
 ppr aa
  = case aa of
  	ALabel s	-> "label = " % show s
	AColor s	-> "color = " % show s
	
