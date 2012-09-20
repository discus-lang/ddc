
module DDC.Desugar.Slurp.SlurpS
	(slurpS)

where
import DDC.Desugar.Slurp.Base
import Source.Desugar			(Annot)
import Data.Bag

slurpS	:: Stmt Annot
	-> CSlurpM (Type, Effect, Closure, Stmt Annot2, Bag CTree)
