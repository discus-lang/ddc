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

padVar = padL 20

-- CTree ------------------------------------------------------------------------------------------
instance Pretty CTree PMode where
 ppr c  
  = case c of
 	CTreeNil
	 -> ppr "NIL"

	CBranch BNothing subs
	 -> "BRANCH\n"
	  % "{\n"
		%> (vcat (map ppr subs))
	  % "}"

	CBranch binds subs
	 -> "BRANCH ("  % binds % ")\n"
	  % "{\n"
		%> (vcat (map ppr subs))
	  % "}"


	CSig	_ v t
	 -> "SIG  " % v % "\n" %> prettyTypeSplit t

	CEq 	_ v t	
	 -> padVar v 	%% "= " %% prettyTypeSplit t

	CEqs 	_ ts
	 -> punc " =  " $ map padVar ts
	
	CClass	_ v ts
	 -> "CLASS " % v %% ts

	CProject _ tp vInst tDict tBind
	 -> "PROJECT " % tp % " " % vInst % " " % tDict %% tBind

	CDictProject _ t vs
	 -> "DICTPROJECT    " % t % "\n"
	 	% "{\n"
		%> ("\n" %!% map (\(v1, v2) -> v1 %>> " = " % v2 % ";") (Map.toList vs) % "\n")
		% "}"
	
	CClassInst _ v ts
	 -> "CLASSINST " % v % " " % ts

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
  	BLet  	  vs	-> "LET"	%% punc " " vs
	BLetGroup vs	-> "LETGROUP"	%% punc " " vs
	BLambda	  vs 	-> "LAMBDA"	%% punc " " vs
  	BDecon 	  vs  	-> "DECON"	%% punc " " vs


