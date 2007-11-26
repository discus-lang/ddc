
module Core.Inline
(
	countUsageTree,
	countUsageX,
	
	spreadUsageTree,
	collectPureSingleTree,
	inlineBindsTree,
	inlineBindsTree_rename,
	
	inlinePureSingleTree,
	
	resnipTree,
	eraseAnnotsTree
)


where

import qualified Data.Map		as Map
import Data.Map				(Map)

import Util
import Shared.VarPrim	
import Shared.VarGen
import Core.Exp
import Core.Util
import Core.Util.Rename
import Core.Util.Slurp

import qualified Core.Plate.Trans	as Trans
import qualified Core.Plate.Walk	as Walk


-----
-- countUsage
--	Counts the number of times each variable occurs in 
--	a Tree/Exp.
--
countUsageTree :: Tree -> Map Var Int
countUsageTree cTree
	= execState 
		(Trans.transformXM countUsageX' cTree) 
		Map.empty


countUsageX ::	Exp	-> Map Var Int
countUsageX xx
 	= execState 
		(Trans.transformXM countUsageX' xx) 
		Map.empty

countUsageX'	xx
 = case xx of
	XVar v	
	 -> do	useMap		<- get
	 	let oldUse	= fromMaybe 0 (Map.lookup v useMap)
		let newUse	= oldUse + 1
		modify (Map.insert v newUse)
		
		return xx
		
	_ -> return xx


-----
-- spreadUsageTree
--	Uses the supplied usage map (obtained via countUsage) to 
--	annotate each statement in a tree with the number of times
--	the bound variable is used.
--
spreadUsageTree :: Map Var Int -> Tree -> Tree
spreadUsageTree table cTree
 	= evalState
		(Trans.transformSM (spreadUsageS' table) cTree)
		()
		
spreadUsageS' table ss
 	| SBind (Just v) x	<- ss
	, Just usageCount	<- Map.lookup v table
	= return
	$ SBind (Just v)  
	$ addXAnnot (NUseCount $ usageCount) x
	
	| otherwise
	= return ss




-----
-- collectPureSingleTree
--	Takes a tree annotated with binding usage information and 
--	collects up all the bindings which are Pure, and who's bound 
--	variable is only used a single time.
--
collectPureSingleTree
	:: Tree 
	-> (Tree, [Stmt])

collectPureSingleTree cTree
 	= runState
	 	(Walk.walkZM 
			Walk.walkTableId { Walk.transSS = collectPureSingleSS }
			cTree)
		[]

collectPureSingleSS table ss
 	= liftM concat
	$ mapM (collectPureSingleS table) ss
	
collectPureSingleS table s
	| SBind mV (XAnnot [NUseCount 1] x)	<- s
	, slurpEffsX x == TBot KEffect
	, canMoveX x
	= do	modify $ \s -> SBind mV x : s
		return []
		
	| otherwise
	= return [s]

hasConstFs fs r
 	= not 
	$ isNil 
	$ [v 	| TClass v [TVar KRegion r'] <- fs 
		, r == r' 
		, v == primConst ]


canMoveX xx
 = case xx of
 	XLam{}	-> False
	XLAM{}	-> False
	_	-> True

-----
-- inlineBindsTree
--	Inline some bindings into a Tree.
--
inlineBindsTree
	:: Map Var Exp
	-> Tree
	-> Tree
	
inlineBindsTree xMap cTree
 	= Trans.transformX (inlineBindsX xMap) cTree
 
inlineBindsX xMap x
 	| XVar v	<- x
	, Just x'	<- Map.lookup v xMap
	= Trans.transformX (inlineBindsX xMap) x'
	
	| otherwise
	= x

-----
-- inlineBindsTree_rename
--	Inline some bindings into a tree
--	Rename the vars in the bound expression before we expand so the vars don't conflict
--	with anything else in scope.
--
inlineBindsTree_rename
	:: Map Var Exp
	-> Tree
	-> State VarGen Tree
	
inlineBindsTree_rename xMap cTree
	= Trans.transformXM (inlineBindsX_rename xMap) cTree
	
inlineBindsX_rename xMap x
	| XVar v	<- x
	, Just x'	<- Map.lookup v xMap
	= do	
		-- rename the vars in the expression being inlined
		varGen			<- get
		let (xRenamed, renameS')	
			= runState 
				(renameBindersX x') 
				RenameS { sVarGen = varGen, sVarMap = Map.empty }
				
		put (sVarGen renameS')
		
		-- The expression being inlined will often have a XTau wrapper,
		--	but so will the var we're inlining into. Eliminate duplicate XTaus
		let xChompedTau		= chompTau xRenamed
		
		Trans.transformXM (inlineBindsX_rename xMap) xChompedTau
	
	| otherwise
	= return x


chompTau :: Exp -> Exp
chompTau x@(XTau t1 (XTau t2 x2))
 | t1 == t2	= XTau t1 x2
 | otherwise	= x
 
chompTau x	= x



-----
-- inlinePureSingleTree
--	Find bindings which are pure, and who's bound variable
--	is only used a single time, and inline them into their 
--	use sites.
--
inlinePureSingleTree
	:: Tree -> Tree
	
inlinePureSingleTree tree
 = let
	useMap		= countUsageTree  tree
	treeUsage	= spreadUsageTree useMap tree

	(treeCollect, sMap) 
			= collectPureSingleTree treeUsage

	xMap		= Map.fromList
			$ map (\(SBind (Just v) x) -> (v, dropXTau2 x))
			$ sMap

	treeInlined	= inlineBindsTree xMap treeCollect
   in	treeInlined


dropXTau2 xx
 = case xx of
 	XTau t x	-> x
	_		-> xx

-----
resnipTree
	:: Tree 
	-> Tree
	
resnipTree cTree
	= Trans.transformSS resnipTreeSS cTree
	
resnipTreeSS ss
	= concat
	$ map resnipTreeS ss
	
resnipTreeS s
 = let	(s', ss)
 		= runState 
			(Trans.transZM 
				Trans.transTableId { Trans.transX = resnipTreeX }
				s)
			[]
   in	(ss ++ [s'])
   
resnipTreeX xx
 = case xx of
 	XAnnot [NBindVar v] x
	 -> do	modify $ \s -> s ++ [SBind (Just v) x]
	 	return	$ XVar v
		
	_	-> return xx


-----
eraseAnnotsTree	:: Tree -> Tree
eraseAnnotsTree	tree
	= Trans.transformX eraseAnnotsX tree
	
eraseAnnotsX xx
	= case xx of
		XAnnot n x	-> x
		_		-> xx
