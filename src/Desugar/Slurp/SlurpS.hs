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

-----
stage	= "Desugar.Slurp.SlurpS"

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
 	let src		= TSStmt sp
	tBind		<- newTVarD
	
	(tX@(TVar _ tXv), eX, cX, x1', qsX)	
			<- slurpX e1
	
	let qs	= 
		[ CEq  src tBind	$ tX ]

	return	( tX
		, eX
		, cX
		, SBind (Just (tX, eX)) Nothing x1'
		, [newCBranch
			{ branchBind	= BNil
			, branchSub	= qs ++ qsX }])

-- regular bindings
slurpS	(SBind sp (Just v) e1)
 = do
	let src			= TSStmt sp 

	tBind@(TVar _ vBindT)	<- lbindVtoT v
	
 	(tX@(TVar _ tXv), eX, cX, x1', qsX)	
			<- slurpX e1

	-- we'll be wanting the type of this binding when we convert to core
	modify (\s -> s { stateTypesPlease = Set.insert vBindT (stateTypesPlease s)  })

	return	( tX
		, eX
		, cX 
		, SBind (Just (tX, eX)) (Just v) x1'
		, [newCBranch
			{ branchBind	= BLet [vBindT]
			, branchSub	
			   	=  [ CEq  src tBind tX ]
				++ qsX  
				++ [ CGen src tBind ] } ] )

-- type signatures
slurpS	stmt@(SSig sp varV t)
 = do
	let src		= TSSig sp varV
 	tX		<- lbindVtoT varV
	
	let qs	= 
		[ CSig src tX 	$ t ]
		
	return	( tX
		, pure
		, empty
		, SSig Nothing varV t
		, qs)
