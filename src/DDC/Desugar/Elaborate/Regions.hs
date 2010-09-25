{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Desugar.Elaborate.Regions
	(elabRegionsInGlob)
where
import DDC.Desugar.Elaborate.State
import DDC.Desugar.Transform
import DDC.Desugar.Glob
import DDC.Desugar.Exp
import DDC.Base.SourcePos
import DDC.Type


-- | Add missing region variables to type signatures in this tree.
--   This just walks down the tree and calls the elaborator from
--   "DDC.Type.Operators.Elaborate" at the appropriate places.
elabRegionsInGlob :: Glob SourcePos -> ElabM (Glob SourcePos)
elabRegionsInGlob glob
	= transZM (transTableId return)
		{ transP	= elabRegionsP
		, transS_leave	= elabRegionsS 
		, transX_leave	= elabRegionsX }
		glob

elabRegionsP pp
 = case pp of
	PExtern sp v t ot
	 -> do	t'	<- elabRegionsT t
		return	$ PExtern sp v t' ot
		
	PClassDecl sp v ts vts
	 -> do	ts'	<- mapM elabRegionsT ts
		let (vs, mts)	= unzip vts
		mts'	<- mapM elabRegionsT mts
		return	$ PClassDecl sp v ts' (zip vs mts')
		
	PClassInst sp v ts ss
	 -> do	ts'	<- mapM elabRegionsT ts
		return	$ PClassInst sp v ts' ss
	
	PProjDict sp t ss
	 -> do	t'	<- elabRegionsT t
		return	$ PProjDict sp t' ss
	
	PTypeSig sp sigMode v t
	 -> do	t'	<- elabRegionsT t
		return	$ PTypeSig sp sigMode v t'
			
	_ ->	return pp


elabRegionsS ss
 = case ss of
	SSig sp sigMode v t
	 -> do	t'	<- elabRegionsT t
		return	$ SSig sp sigMode v t'

	_		-> return ss


elabRegionsX xx
 = case xx of
	XProjT sp t j
	 -> do	t'	<- elabRegionsT t
		return	$ XProjT sp t' j
	
	_ ->	return xx

elabRegionsT t
 = do	(t_elab, _)	<- elaborateRsT newVarN t
   	return t_elab
