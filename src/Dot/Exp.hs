
module Dot.Exp
	( Graph(..)
	, Stmt (..)
	, NodeId (..)
	, Attr (..))
where
import Shared.Var		(Var)


data Graph
	= Graph 	[Stmt]
	| DiGraph	[Stmt]
	deriving (Show, Eq)
	
	
data Stmt
	= SEdge NodeId NodeId 
	| SNode NodeId [Attr]
	deriving (Show, Eq)
	
	
data NodeId
	= NVar 		Var
	| NString	String
	deriving (Show, Eq)
	

data Attr
	= ALabel String
	| AColor String
	deriving (Show, Eq)

