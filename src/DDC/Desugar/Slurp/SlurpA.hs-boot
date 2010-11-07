
module DDC.Desugar.Slurp.SlurpA
	(slurpA)
where
import DDC.Desugar.Slurp.Base

slurpA	:: Alt Annot1	
	-> CSlurpM 
		( Type					-- type constraint placed on the case object.
		, Type					-- type of the RHS.
		, Effect				-- effect of evaluating the alternative.
		, Closure				-- closure of alternative.
		, Alt Annot2				-- annotated Alt.
		, CTree)				-- constraints.
