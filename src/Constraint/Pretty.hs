-- | Pretty printer for type constraints.
--
module Constraint.Pretty
	( )
where

-----
import Util

import qualified Data.Map	as Map
import Type.Pretty
import Constraint.Exp

-----
instance Pretty CTree where
 prettyp c  
  = case c of
 	CTreeNil
	 -> prettyp "@CTreeNil"

	CBranch{}
	 -> "@CBranch ("  % branchBind c % ")\n"
	  % "{\n"
		%> ("\n" %!% branchSub c % "\n")
	  % "}\n"

	CDef 	src v t	
	 -> "@CDef " % padR 15 (pretty v) % "\n" %> (":: " % prettyTypeSplit t) % ";\n"

	CSig	src v t
	 -> "@CSig  " % padR 15 (pretty v) % " " % prettyTB t % ";"

	CEq 	src v t	
	 -> "@CEq   " % padR 15 (pretty v) % " " % prettyTB t % ";"

	CEqs 	src ts
	 -> "@CEqs  " % ts

	CClass	src v ts
	 -> "@CClass " % v % " " % ts

	CProject src tp vInst tDict tBind
	 -> "@CProject " % tp % " " % vInst % " " % tDict % " " % tBind % ";\n"

	CDataFields src v ts []	
	 -> "@CDataFields " % v % " " % ts % " {};"
	 
	CDataFields src v ts fs
	 -> "@CDataFields " % v % " " % ts % "\n"
		% "{\n"
		%> ("\n " %!% map (\(v, f) -> v %>> " :: " % f) fs % "\n") 
		% "};\n\n"

	CDictProject src t vs
	 -> "@CDictProject    " % t % "\n"
	 	% "{\n"
		%> ("\n" %!% map (\(v1, v2) -> v1 %>> " = " % v2 % ";") (Map.toList vs) % "\n")
		% "};\n\n"
	
	CClassInst src v ts
	 -> "@ClassInst " % v % " " % ts % ";"

	CLeave v
	 -> "@CLeave " % v % ";"

	CInst src v1 v2
	 -> "@CInst " % padR 15 (pretty v1) % " " % v2 % ";"
	 
	CGen src v1
	 -> "@CGen  " % v1 % ";"
	 
	CTopEffect t
	 -> "@CTopEffect " % t		% ";"
	 
	CTopClosure t
	 -> "@CTopClosure " % t	 	% ";"
 

-----
instance Pretty CBind where
 prettyp bb	
  = case bb of
	BNil		-> prettyp "@BNil"
  	BLet 	v	-> "@BLet "		% v
	BLetGroup vs	-> "@BLetGroup "	% vs
	BLambda	v 	-> "@BLambda "		% v
  	BDecon	v  	-> "@BDecon "		% v
