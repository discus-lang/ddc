{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Pretty printing of type constraints.
module DDC.Constraint.Pretty
	()
where
import DDC.Constraint.Exp
import DDC.Type
import DDC.Main.Pretty
import qualified Data.Map	as Map


-- CTree ------------------------------------------------------------------------------------------
instance Pretty CTree PMode where
 ppr c  
  = case c of
 	CTreeNil
	 -> ppr "@CTreeNil"

	CBranch{}
	 -> "@CBranch ("  % branchBind c % ")\n"
	  % "{\n"
		%> ("\n" %!% branchSub c % "\n")
	  % "}\n"

	CSig	_ v t
	 -> "@CSig  " % v % "\n" %> prettyTypeSplit t % ";\n"

	CEq 	_ v t	
	 -> "@CEq   " % padL 15 v % " " % prettyTypeSplit t % ";"

	CEqs 	_ ts
	 -> "@CEqs  " % ts

	CClass	_ v ts
	 -> "@CClass " % v % " " % ts

	CProject _ tp vInst tDict tBind
	 -> "@CProject " % tp % " " % vInst % " " % tDict % " " % tBind % ";\n"

	CDictProject _ t vs
	 -> "@CDictProject    " % t % "\n"
	 	% "{\n"
		%> ("\n" %!% map (\(v1, v2) -> v1 %>> " = " % v2 % ";") (Map.toList vs) % "\n")
		% "};\n\n"
	
	CClassInst _ v ts
	 -> "@ClassInst " % v % " " % ts % ";"

	CInst _ v1 v2
	 -> "@CInst " % padL 15 v1 % " " % v2 % ";"
	 
	CGen _ v1
	 -> "@CGen  " % v1 % ";"
	 
	-- internal
	CLeave v
	 -> "@CLeave " % v % ";"
 
 	CGrind
	 -> ppr "@CGrind"

	CInstLambda _ v1 v2
	 -> "@InstLambda " % v1 % " " % v2

	CInstLet _ v1 v2
	 -> "@InstLet " % v1 % " " % v2

	CInstLetRec _ v1 v2
	 -> "@InstLetRec " % v1 % " " % v2
	 
	 


-- CBind ------------------------------------------------------------------------------------------
instance Pretty CBind PMode where
 ppr bb	
  = case bb of
	BNothing	-> ppr "@BNothing"
  	BLet 	v	-> "@BLet "		% v
	BLetGroup vs	-> "@BLetGroup "	% vs
	BLambda	v 	-> "@BLambda "		% v
  	BDecon	v  	-> "@BDecon "		% v


