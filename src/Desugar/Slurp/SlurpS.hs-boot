
module Desugar.Slurp.SlurpS
	(slurpS)

where
import Desugar.Slurp.Base

slurpS	:: Stmt Annot1	
	-> CSlurpM (Type, Effect, Closure, Stmt Annot2, [CTree])
