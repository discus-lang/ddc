{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Node types are the simple type constraints that are stored directly in graph equivalence
--   classes. The children of a Node type are always cids. This is opposed to regular 
--   Types who's children can be more types.
--
--  HISTORY: We use to just have regular types in the graph, but its too fiddly to deal
--           with the possibility of children being either cids or more types. It's a lot
--           less buggy to keep the graph in this simpler form.
--
module DDC.Solve.Node
	( Node		(..)

	-- * Simple checks.
	, isNVar
	, isNBot
	, isNSum

	-- * (de)Construction
	, makeNSum
	, takeNSum

	-- * Substitution.
	, subNodeCidCid

	-- * Extraction.
	, cidsOfNode
	
	-- * Builtin nodes.
	, nBot
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

	-- A closure constructor. 
	-- TODO: I'm not sure if we want to convert the right of this to node form
	--       as well. We'd need a TyCon for the (NFree v) part first though	
	| NFree		Var Type
	
	-- | Used when the node has been involved in a type error.
	| NError
	deriving (Show, Eq)


-- Simple Checks ----------------------------------------------------------------------------------
-- | Check if a node is an NVar.
isNVar :: Node -> Bool
isNVar nn
 = case nn of
	NVar{}	-> True
	_	-> False


-- | Check if a node is represents bottom.
isNBot :: Node -> Bool
isNBot nn 
 = case nn of
	NBot		-> True
	NSum ss		-> Set.null ss
	_		-> False

-- | Check whether a node is an NSum.
isNSum :: Node -> Bool
isNSum nn	= isJust $ takeNSum nn


-- (de)Construction -------------------------------------------------------------------------------
-- | Make a new NSum containing these cids, or a NBot if the set is empty.
makeNSum :: Set ClassId -> Node
makeNSum cids
	| Set.null cids	= NBot
	| otherwise	= NSum cids

-- | Take the cids from an NSum.
takeNSum :: Node -> Maybe (Set ClassId)
takeNSum nn
 = case nn of
	NSum cids	-> Just cids
	_		-> Nothing
	
	
-- Substitution -----------------------------------------------------------------------------------
-- | Substitute cids for cids in some node
subNodeCidCid :: Map ClassId ClassId -> Node -> Node
subNodeCidCid sub nn
 = case nn of
	NBot{}		-> nn
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


-- Extraction -------------------------------------------------------------------------------------
-- | Get the set of all ClassIds in a node.
cidsOfNode :: Node -> Set ClassId
cidsOfNode nn
 = case nn of
	NBot		-> Set.empty
	NVar{}		-> Set.empty
	NCon{}		-> Set.empty
	NApp c1 c2	-> Set.fromList [c1, c2]
	NSum cs		-> cs
	NError{}	-> Set.empty
	NScheme t	-> collectClassIds t
	NFree v t	-> collectClassIds t


-- Builtins ---------------------------------------------------------------------------------------
nBot		= NBot

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


-- Instances --------------------------------------------------------------------------------------
instance Pretty Node PMode where
 ppr nn
  = case nn of
	NBot		-> ppr "NBot"
	NVar v		-> ppr v
	NCon tc		-> ppr tc
	NApp cid1 cid2	-> parens $ cid1 <> cid2
	NScheme t	-> "NScheme " % t
	NFree v t	-> "NFree " % v % " :: " % t
	_		-> ppr $ show nn

