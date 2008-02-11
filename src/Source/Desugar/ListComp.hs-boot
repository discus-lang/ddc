
module Source.Desugar.ListComp
(
	rewriteListComp
)

where

import Shared.Base
import qualified Source.Exp		as S
import qualified Desugar.Exp		as D

import Source.Desugar.Base

rewriteListComp 
	:: S.Exp SourcePos -> RewriteM (D.Exp Annot)
