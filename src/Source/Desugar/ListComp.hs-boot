
module Source.Desugar.ListComp
	(rewriteListComp)
where
import DDC.Base.SourcePos
import Source.Desugar.Base
import qualified Source.Exp		as S
import qualified Desugar.Exp		as D

rewriteListComp 
	:: S.Exp SourcePos -> RewriteM (D.Exp Annot)
