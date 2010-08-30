{-# OPTIONS -fno-warn-monomorphism-restriction #-}
-- | Elaborate data definitions.
module Desugar.Data 
	( elaborateData
	, elaborateTypeSynonym )
where
import Desugar.Pretty
import DDC.Desugar.Exp
import Shared.VarPrim
import DDC.Base.SourcePos
import DDC.Base.DataFormat
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Type.Data
import DDC.Var
import Util
import qualified Debug.Trace

stage		= "Desugar.Data"
debug		= False
trace s xx	= if debug then Debug.Trace.trace (pprStrPlain s) xx else xx


elaborateTypeSynonym 
	:: Monad m
	=> (NameSpace 	-> m Var)
	-> (Var		-> m Kind)
	-> Top SourcePos -> m (Top SourcePos)

elaborateTypeSynonym newVarN getKind 
	p@(PTypeSynonym sp vData typ)
 = do
	trace 	( "elaborateTypeSynonym\n"
		% "    in:\n" %> stripAnnot p	% "\n")
		$ return ()

	let ?newVar	= newVarN
	let ?getKind	= getKind

	panic stage "Sorry don't handle PTypeSynonym yet!\n"


