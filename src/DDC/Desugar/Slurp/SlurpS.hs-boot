
module DDC.Desugar.Slurp.SlurpS
	(slurpS)

where
import DDC.Desugar.Slurp.Base
import Data.Sequence

slurpS	:: Stmt Annot1	
	-> CSlurpM (Type, Effect, Closure, Stmt Annot2, Seq CTree)
