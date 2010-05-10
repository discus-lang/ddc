
module DDC.Solve.Node
	( Node		(..)
	, isNVar
	, subNodeCidCid
	, cidsOfNode
	
	, nRead
	, nDeepRead
	, nHeadRead
	, nWrite
	, nDeepWrite)
where
import Type.Exp
import Type.Builtin
import Type.Plate.Collect
import Type.Util
import DDC.Var
import DDC.Main.Pretty
import Data.Maybe
import Type.Pretty		()
import Data.Set			(Set)
import Data.Map			(Map)
import qualified Data.Set	as Set
import qualified Data.Map	as Map


-- | A node type.
--	These are the simple type constraints stored directly in the graph nodes.
--	We keep them separate from Type.Exp because all of their components need to be ClassIds.
data Node
	= NBot	
	| NVar		Var
	| NCon		TyCon
	| NApp		ClassId	ClassId
	| NSum		(Set ClassId)

	-- | Finished type schemes are added back to the type graph.
	--	We want to keep them in the graph because closure terms in other types
	--	may be refering to them, and they may still contain unquantified cids.
	| NScheme 	Type
	
	-- | Errors
	| NError

	-- Closure constructor. TODO: factor this into NApp.
	| NFree		Var Type
	deriving (Show, Eq)


-- | Check if a node is an NVar.
isNVar :: Node -> Bool
isNVar nn
 = case nn of
	NVar{}	-> True
	_	-> False

-- | Get the set of all ClassIds in a node.
cidsOfNode :: Node -> Set ClassId
cidsOfNode nn
 = case nn of
	NBot{}		-> Set.empty
	NVar{}		-> Set.empty
	NCon{}		-> Set.empty
	NApp c1 c2	-> Set.fromList [c1, c2]
	NSum cs		-> cs
	NError{}	-> Set.empty
	NScheme t	-> collectClassIds t
	NFree v t	-> collectClassIds t


-- | Substitute cids for cids in some node
subNodeCidCid :: Map ClassId ClassId -> Node -> Node
subNodeCidCid sub nn
 = case nn of
	NBot		-> nn
	NVar{}		-> nn
	NCon{}		-> nn

	NApp cid1 cid2
	 -> NApp  (fromMaybe cid1 (Map.lookup cid1 sub))
		  (fromMaybe cid2 (Map.lookup cid2 sub))
	
	NSum cids
	 -> NSum $ Set.map (\cid -> fromMaybe cid (Map.lookup cid sub)) cids
	
	NScheme t
	 -> NScheme $ subCidCid sub t
	
	NError{}	-> nn
	
	NFree v t	
	 -> NFree v $ subCidCid sub t


	
instance Pretty Node PMode where
	ppr nn
	 = case nn of
		NScheme t	-> "NScheme " % t
		NFree v t	-> "NFree " % v % " :: " % t
		_		-> ppr $ show nn

	
-- | Effect type constructors (in node form)
nRead		= NCon $ TyConEffect TyConEffectRead
		$ KFun kRegion kEffect

nDeepRead	= NCon $ TyConEffect TyConEffectDeepRead
		$ KFun kValue kEffect

nHeadRead	= NCon $ TyConEffect TyConEffectHeadRead
		$ KFun kValue kEffect

nWrite		= NCon $ TyConEffect TyConEffectWrite
		$ KFun kRegion kEffect

nDeepWrite	= NCon $ TyConEffect TyConEffectDeepWrite
		$ KFun kValue kEffect



