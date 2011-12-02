{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Desugar.ProjectEta
	(projectEtaExpandTree)
where
import DDC.Desugar.Exp
import DDC.Desugar.Bits
import DDC.Var
import Source.Desugar		(Annot)
import Shared.VarGen
import Control.Monad
import qualified Data.Map	as Map
import Util


-- | Eta expand projection functions
--	This helps avoid intermediate partial of the projection's 'worker' function.
--
-- Eg change
-- 	project Type where
--		fun1 = fun1_impl
--		fun2 = fun2_impl
--
--	fun1_impl1 x y = ...
--	fun2_impl2 x   = ...
--
-- into:
--
--	project Type where
--		fun1 = \x y -> fun1_impl x y
--		fun2 = \x   -> fun2_impl x
--
--	fun1_impl1 x y = ...
--	fun2_impl2 x   = ...
--
projectEtaExpandTree
	:: String		-- ^ unqiue bind
	-> Tree Annot		-- ^ source tree
	-> Tree Annot

projectEtaExpandTree unique tree
 = let	arities	= slurpArity tree
   in	evalVarGen (mapM (projectEtaP arities) tree) ("x" ++ unique)


projectEtaP :: Map Var Int -> Top Annot -> VarGenM (Top Annot)
projectEtaP table pp
 = case pp of
 	PProjDict n t ss
	 -> do	ss'	<- mapM (projectEtaS table) ss
	 	return	$ PProjDict n t ss'

	_ ->	return pp


-- | Eta explain variable bindings.
projectEtaS :: Map Var Int -> Stmt Annot -> VarGenM (Stmt Annot)
projectEtaS table ss
 	| SBind n1 mV (XVar n2 v2)	<- ss
	, Just arity	<- Map.lookup v2 table
	= do 	x'	<- etaExpand arity (XVar n2 v2)
		return	$ SBind n1 mV x'

	| otherwise
	= return ss


-- | Eta expand this expression
etaExpand
	:: Int 		-- ^ number of args to add
	-> Exp Annot	-- ^ expression
	-> VarGenM (Exp Annot)

etaExpand args xx
 = do	vars		<- replicateM args (newVarN NameValue)
	let Just n	= takeAnnotX xx

	let xxApp	= foldl (\x v -> XApp n x (XVar n v)) 	xx 	vars
	let xxLam	= foldl (\x v -> XLambda n v x)		xxApp	(reverse vars)

	return xxLam


-- | Slurp out a map of arities for each of the top level bindings in this tree.
slurpArity :: Tree Annot -> Map Var Int
slurpArity tree
 = foldl' slurpArityP Map.empty tree

slurpArityP :: Map Var Int -> Top Annot -> Map Var Int
slurpArityP table pp
 = case pp of
 	PBind _ v x	-> Map.insert v (arityX x) table
	_		-> table


-- | Determine the binding arity of an expression.
arityX :: Exp Annot -> Int
arityX xx
 = case xx of
 	XLambda _ _ x	-> 1 + arityX x
	_		-> 0

