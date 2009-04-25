
-- | Bits and pieces for dealing with type constraints.
module Constraint.Bits
	( isCBranch
	, takeCBindVs
	, mergeCBinds
	, slurpContains)

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Type.Exp
import Constraint.Exp


-- | Check if this is a branch in the constraint tree.
isCBranch :: CTree -> Bool
isCBranch b
 = case b of
 	CBranch{}	-> True
	_		-> False
	
	
-- | Take all the variables bound by term that this constraint branch is of.
takeCBindVs :: CBind -> [Var]
takeCBindVs cc
 = case cc of
	BNil		-> []
	BLet    vs	-> vs
 	BLetGroup  vs	-> vs

	BLambda vs	-> vs
	BDecon  vs	-> vs


-- | Merge the variables in two similar CBinds into another.
mergeCBinds :: CBind -> CBind -> CBind
mergeCBinds (BLet    	vs1) (BLet    	vs2)	= BLet	  	(vs1 ++ vs2)
mergeCBinds (BLetGroup  vs1) (BLetGroup vs2) 	= BLetGroup  	(vs1 ++ vs2)
mergeCBinds (BLambda 	vs1) (BLambda 	vs2)	= BLambda 	(vs1 ++ vs2)
mergeCBinds (BDecon  	vs1) (BDecon  	vs2) 	= BDecon  	(vs1 ++ vs2)


-- | Slurp the containment map from this tree.
--	This is a map of all the variables bound by sub-branches in this tree.
--
--	TODO: A more efficient implementation, not using catMap.
--
slurpContains :: CTree -> Map CBind (Set CBind)
slurpContains tree
	= Map.fromList 
	$ map (\(v, vs) -> (v, Set.fromList vs))
	$ gather
	$ slurpContains' Nothing tree


slurpContains' :: Maybe CBind -> CTree -> [ (CBind, CBind) ]
slurpContains' mParent tree@(CBranch{})

	| BNil			<- boundVsT 
	= catMap (slurpContains' mParent) (branchSub tree)

 	| Nothing		<- mParent
	= catMap (slurpContains' (Just boundVsT)) (branchSub tree)
	
	| Just parent		<- mParent
	= (parent, boundVsT) 
	: catMap (slurpContains' (Just boundVsT)) (branchSub tree)
	
	where	boundVsT	= branchBind tree

slurpContains' mParent _
	= []
	