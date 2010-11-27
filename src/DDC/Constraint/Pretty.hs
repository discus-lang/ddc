{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Pretty printing of type constraints.
module DDC.Constraint.Pretty
	()
where
import DDC.Constraint.Exp
import DDC.Type
import DDC.Main.Pretty
import qualified Data.Map	as Map
import qualified Data.Foldable	as Seq

padVar = padL 20

-- CTree ------------------------------------------------------------------------------------------
instance Pretty CTree PMode where
 ppr c  
  = case c of
 	CTreeNil
	 -> ppr "NIL"

	CBranch BNothing subs
	 -> pprHeadBlock ("BRANCH" % nl) 
		$ Seq.toList subs

	CBranch binds subs
	 -> pprHeadBlock ("BRANCH" %% binds % nl) 
		$ Seq.toList subs
	
	CEq 	_ v t	
	 -> padVar v 	%% "= " %% prettyTypeSplit t

	CEqs 	_ ts
	 -> punc " =  " $ map padVar ts
	
	CMore	_ v t
	 -> padVar v	%% ":>" %% prettyTypeSplit t
	
	CProject _ tp vInst tDict tBind
	 -> "PROJECT " % tp % " " % vInst % " " % tDict %% tBind

	CDictProject _ t vs
	 -> pprHeadBlock ("DICTPROJECT    " % t)
		[ v1 %>> " = " % v2 | (v1, v2)	<- Map.toList vs ]
	
	CInst _ v1 v2
	 -> padVar v1	%% "<- INST" %% v2
	 
	CGen _ v1
	 -> "GEN  " % v1
	 
	-- internal
	CLeave v
	 -> "LEAVE " % v
 
 	CGrind
	 -> ppr "GRIND"

	CInstLambda _ v1 v2
	 -> "INSTLAMBDA " % v1 %% v2

	CInstLet _ v1 v2
	 -> "INSTLET " 	  % v1 %% v2

	CInstLetRec _ v1 v2
	 -> "INSTLETREC " % v1 %% v2


-- CBind ------------------------------------------------------------------------------------------
instance Pretty CBind PMode where
 ppr bb	
  = case bb of
	BNothing	-> ppr "NOTHING"
  	BLet  	  vs	-> punc " " (ppr "LET"		: map ppr vs)
	BLetGroup vs	-> punc " " (ppr "LETGROUP" 	: map ppr vs)
	BLambda	  vs 	-> punc " " (ppr "LAMBDA"	: map ppr vs)
  	BDecon 	  vs  	-> punc " " (ppr "DECON"	: map ppr vs)

