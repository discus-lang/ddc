-- | Constraint slurper for statements.
--
module Desugar.Slurp.SlurpS 
	(slurpS)
where

-----
import Util

import qualified Shared.Var	as Var
import qualified Data.Set	as Set

import Desugar.Slurp.Base
import Desugar.Slurp.SlurpX
import Desugar.Slurp.SlurpA

import Type.Location

-----
-- stage	= "Desugar.Slurp.SlurpS"

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
			{ branchBind	= BNil
			, branchSub	= qs ++ qsX }])

-- regular bindings
slurpS	(SBind sp (Just v) e1)
 = do
	tBind@(TVar _ vBindT)	<- lbindVtoT v
	
 	(tX@(TVar _ tXv), eX, cX, x1', qsX)	
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



