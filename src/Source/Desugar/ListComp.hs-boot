
module Source.Desugar.ListComp
(
	rewriteListComp
)

where

import qualified Source.Exp		as S
import qualified Desugar.Exp		as D

import Source.Desugar.Base

rewriteListComp 
	:: S.Exp -> RewriteM (D.Exp Annot)
