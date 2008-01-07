
module Constraint.Bits
	( isCBranchLet
	, isCDef
	, isCDictProject
	, isCDataFields
	, isCSig
	, isCClassInst

	, takeCVar
	, takeCType
	, takeCBindVs
	, mergeCBinds
	, liftCType
	, slurpInstEnv 
	, slurpContains)

where

import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Type.Exp
import Constraint.Exp


-- Constructor predicates.
--	not very interesting.
--
isCBranchLet b
	| CBranch{}	<- b
	, BLet{}	<- branchBind b
	= True
	
	| otherwise
	= False

isCDef b
 = case b of
 	CDef{}		-> True
	_		-> False
	
isCDictProject b
 = case b of
 	CDictProject{}	-> True
	_		-> False

isCDataFields b
 = case b of
 	CDataFields{}	-> True
	_		-> False

isCSig b
 = case b of
 	CSig{}		-> True
	_		-> False

isCClassInst b
 = case b of
 	CClassInst{}	-> True
	_		-> False
	

-----
takeCVar x	
 = case x of
 	CEq   _ (TVar _ v) _	-> v
--	CGen  _ v _		-> v
	CDef  _ (TVar _ v) _	-> v

takeCType x
 = case x of
 	CEq   _ _ t	-> t
	CDef  _ _ t	-> t

liftCType f x
 = case x of
 	CEq     ts v t	-> CEq   ts v (f t)
	CDef    ts v t	-> CDef  ts v (f t)


--
takeCBindVs :: CBind -> [Var]
takeCBindVs cc
 = case cc of
	BNil		-> []
	BLet    vs	-> vs
 	BLetGroup  vs	-> vs

	BLambda vs	-> vs
	BDecon  vs	-> vs

mergeCBinds :: CBind -> CBind -> CBind
mergeCBinds (BLet    	vs1) (BLet    	vs2)	= BLet	  	(vs1 ++ vs2)
mergeCBinds (BLetGroup  vs1) (BLetGroup vs2) 	= BLetGroup  	(vs1 ++ vs2)
mergeCBinds (BLambda 	vs1) (BLambda 	vs2)	= BLambda 	(vs1 ++ vs2)
mergeCBinds (BDecon  	vs1) (BDecon  	vs2) 	= BDecon  	(vs1 ++ vs2)


-- returns a list of the environment of a constraint.
--	
slurpInstEnv :: CTree -> [(Var, Var)]
slurpInstEnv tree
 = case tree of
 	CBranch{}
	 -> let boundVsT	= takeCBindVs $ branchBind tree
	    in	[ (vInst, vDef)
	    		| (vInst, vDef)	<- catMap slurpInstEnv (branchSub tree) 
			, not $ elem vDef boundVsT ]

	CEq{}			-> []
	CEqs{}			-> []
	CInst ts vInst vDef	-> [(vInst, vDef)]
	CGen{}			-> []	


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
	
	
chainLinks :: [a] -> [(a, a)]
chainLinks []		= []
chainLinks (x1:[])	= []
chainLinks (x1:x2:xs)	= (x1, x2) : chainLinks (x2 : xs)



					






