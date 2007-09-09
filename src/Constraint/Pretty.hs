
module Constraint.Pretty
(

)

where

-----
import Util

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


	CEq 	src v t	
	 -> "@CEq   " % padR 15 (pretty v) % " " % prettyTB t % ";"

	CEqs 	src ts
	 -> "@CEqs  " % ts

	CDef 	src v t	
	 -> "@CDef        " % padR 15 (pretty v) % " " % prettyTB t % ";"

	CDataFields src v ts []	
	 -> "@CDataFields " % v % " " % ts % " {};"
	 
	CDataFields src v ts fs
	 -> "@CDataFields " % v % " " % ts % "\n"
		% "{\n"
		%> ("\n " %!% map (\(v, f) -> v %>> " :: " % f) fs % "\n") 
		% "};\n\n"

	CProject src t []
	 -> "@CProject    " % t % " {};"

	CProject src t vs
	 -> "@CProject    " % t % "\n"
	 	% "{\n"
		%> ("\n" %!% map (\(v1, v2) -> v1 %>> " = " % v2 % ";") vs % "\n")
		% "};\n\n"
	
	CClassInst src v ts
	 -> "@ClassInst " % v % " " % ts % ";\n"

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
