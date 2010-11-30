{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Desugar.Slurp.SlurpS 
	(slurpS)
where
import DDC.Desugar.Slurp.Base
import DDC.Desugar.Slurp.SlurpX
import DDC.Solve.Location
import DDC.Solve.Interface.Problem
import Util
import qualified Data.MapUtil	as Map
import qualified Data.Bag	as Bag
import Data.Bag			(Bag)

stage	= "DDC.Desugar.Slurp.SlurpS"

-- | Slurp out type constraints a statement.
slurpS 	:: Stmt Annot1
	-> CSlurpM 
		( Type		-- type var
		, Effect	-- effect vars
		, Closure	-- closure of this statement
		, Stmt Annot2	-- annotated statement
		, Bag CTree)	-- constraints

-- statements (bindings with out a bound var)
slurpS 	(SBind sp Nothing e1)
 = do
	tBind				<- newTVarD
	(tX@TVar{}, eX, _, x1', qsX)	<- slurpX e1
	
	let qs = [ CEq  (TSU $ SUBind sp) tBind	$ tX ]

	return	( tX
		, eX
		, tEmpty
		, SBind (Just (tX, eX)) Nothing x1'
		, Bag.singleton
			$ CBranch
			{ branchBind	= BNothing
			, branchSub	= qs ++ Bag.toList qsX } )

-- regular bindings
slurpS	(SBind sp (Just v) e1)
 = do
	tBind@(TVar _ (UVar vBindT))		<- lbindVtoT v
 	(tX@(TVar _ (UVar{})), eX, _, x1', qsX)	<- slurpX e1

	return	( tX
		, eX
		, tEmpty
		, SBind (Just (tX, eX)) (Just v) x1'
		, Bag.singleton 
			$ CBranch
			{ branchBind	= BLet [vBindT]
			, branchSub	
			   	=  [ CEq  (TSU $ SUBind sp) tBind tX ]
				++ Bag.toList (qsX >< (Bag.singleton (CGen (TSM $ SMGen sp v) tBind))) })

-- type signatures
slurpS	(SSig sp sigMode vs tSig)
 = do
	forM_ vs 
	 $ \v -> do	
		TVar _ (UVar vT) <- lbindVtoT v
		let sig	= ProbSig v sp sigMode tSig
		modify $ \s -> s { 
			stateSlurpSigs = Map.adjustWithDefault (++ [sig]) [] vT (stateSlurpSigs s) }

	(tX1 : _)	<- mapM lbindVtoT vs
 		
	return	( tX1
		, tPure
		, tEmpty
		, SSig Nothing sigMode vs tSig
		, Bag.empty)

slurpS	_
	= panic stage
	$ "slurpS: unexpected statement during slurping. Maybe the desugarer messed up."
	
