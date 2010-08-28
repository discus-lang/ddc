-- | Constraint slurper for statements.
--
module Desugar.Slurp.SlurpS 
	(slurpS)
where
import Desugar.Slurp.Base
import Desugar.Slurp.SlurpX
import DDC.Solve.Location

-- | Slurp the type constraints for this statement.
slurpS 	:: Stmt Annot1
	-> CSlurpM 
		( Type		-- type var
		, Effect	-- effect vars
		, Closure	-- closure of this statement
		, Stmt Annot2	-- annotated statement
		, [CTree])	-- constraints

-- statements (bindings with out a bound var)
slurpS 	(SBind sp Nothing e1)
 = do
	tBind		<- newTVarD
	
	(tX@(TVar _ tXv), eX, cX, x1', qsX)	
			<- slurpX e1
	
	let qs	= 
		[ CEq  (TSU $ SUBind sp) tBind	$ tX ]

	return	( tX
		, eX
		, tEmpty
		, SBind (Just (tX, eX)) Nothing x1'
		, [CBranch
			{ branchBind	= BNothing
			, branchSub	= qs ++ qsX }])

-- regular bindings
slurpS	(SBind sp (Just v) e1)
 = do
	tBind@(TVar _ (UVar vBindT))	<- lbindVtoT v
	
 	(tX@(TVar _ (UVar tXv)), eX, cX, x1', qsX)	
			<- slurpX e1

	return	( tX
		, eX
		, tEmpty
		, SBind (Just (tX, eX)) (Just v) x1'
		, [CBranch
			{ branchBind	= BLet [vBindT]
			, branchSub	
			   	=  [ CEq  (TSU $ SUBind sp) tBind tX ]
				++ qsX  
				++ [ CGen (TSM $ SMGen sp v) tBind ] } ] )

-- type signatures
slurpS	stmt@(SSig sp vs t)
 = do
 	tXs@(tX1 : _)	<- mapM lbindVtoT vs
	
	let qs	= 
		[ CSig (TSV $ SVSig sp varV) tX	$ t 
			| tX   <- tXs 
			| varV <- vs ]
		
	return	( tX1
		, tPure
		, tEmpty
		, SSig Nothing vs t
		, qs)



