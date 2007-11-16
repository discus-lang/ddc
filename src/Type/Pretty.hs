{-# OPTIONS -fwarn-incomplete-patterns #-}

module Type.Pretty
	( prettyTB
	, prettyVK
	, prettyTypeSplit
	, prettyTS)

where

-----
import Util

-----
import qualified Shared.Var 	as Var
import Shared.Error
import Type.Exp
import Type.Util.Bits
import Type.Util.Pack

-----
stage	= "Type.Pretty"

-----
-- pretty Type
--
instance Pretty Type where
 prettyp xx
  = case xx of
 	TNil			-> prettyp "@TNil"

	TForall vs t		-> "forall " % " " %!% (map prettyVK vs) % ". " % t
	TFetters fs t		-> t % " :- " % ", " %!% fs

	TUnify k  ts		-> k  % "<" % "; " %!% ts % ">"
	TSum   k  es		-> k  % "{" % "; " %!% es % "}"
	TMask  k  t1 t2		-> prettyTB t1 % " \\ " % prettyTB t2

	TVar k v		-> prettyp v

	TTop k			-> k % "Top"
	TBot k			-> k % "Bot"


	-- data
	TData v []		-> prettyp v 
	TData v ts		-> v % " " % " " %!% (map prettyTB ts)

	TFun t1 t2 eff clo
	 -> case (eff, clo) of
	 	(TBot _ ,	TBot _)		-> prettyTBF t1 % " -> " % t2
		(eff,		TBot _)		-> prettyTBF t1 % " -(" % eff % ")> " % t2
		(TBot _,	clo)		-> prettyTBF t1 % " -(" % clo % ")> " % t2
		(eff,		clo)		-> prettyTBF t1 % " -(" % prettyTB eff % " " % prettyTB clo % ")> " % t2
		
	-- effect
	TEffect    v []		-> prettyp v
	TEffect    v ts		-> v % " " % " " %!% map prettyTB ts

	-- closure
	TFree  v t		-> v % " : " % t
	TTag v			-> prettyp v
	
	-- wild cards
	TWild k			-> k % "_"


	---- used in the type solver
	TClass k c		-> k % c
	TAccept t		-> "@Accept " % prettyTB t
	TNode  c t		-> c % ": " % t
	TFetter f		-> "@TFetter " % f
	TError			-> prettyp "@TError"


	-- used in constraint generator
--	TInstLet v		-> "@TInstLet " % v
--	TUnify ts		-> "@TUnify " % ts
	

	-----
	TElaborate t	-> "elaborate " % t
	TMutable   t	-> "mutable " 	% t 

	-----

{-
	TInst vDef vInst t
	 -> "@TInst " % vDef % " " % vInst % " " % t 

	TField v t	-> "@TField " % "(" % v % " :: " % t % ")"
	TLiteral c t	-> "@TLiteral " % c % " " % prettyTB t
	TIfObj t	-> "@TIfObj " % prettyTB t
-}
	
	-----
--	TProj 	j t e c	-> "@TProj " % j % " " % prettyTB t % " " % e % " " % c


	-----
	TFunF tEs	-> prettyp tEs

	TFunV t1 t2 mV
	 -> case mV of
	 	Nothing
		 -> prettyTBF t1 % " -> " % t2
	
		Just l
		 -> prettyTBF t1 % " -" % l % "> " % t2

	

instance Pretty ClassId where
 prettyp c
  = case c of
  	ClassId i	-> prettyp i

-----
prettyTBF t
 = case t of
 	TFun{}		-> "(" % t % ")"
	TMutable{}	-> "(" % t % ")"
	
	_ 		-> prettyp t

prettyTB t
 = case t of
 	TData v []	-> prettyp t
	TVar k v 	-> prettyp t
	TSum{}		-> prettyp t
	TEffect v []	-> prettyp t
	TTag v		-> prettyp t
	TWild{}		-> prettyp t
	TClass{}	-> prettyp t
	TBot{}		-> prettyp t
	TTop{}		-> prettyp t
	_		-> "(" % t % ")"

----
instance Pretty TProj where
 prettyp p
  = case p of
  	TJField  v	-> "." % v
	TJFieldR v	-> "#" % v


----
prettyVK ::	(Var, Kind)	-> PrettyP
prettyVK	(var, kind)
 = case kind of
	KData		-> prettyp var
	KRegion		-> prettyp var
	KEffect		-> prettyp var
	KClosure	-> prettyp var
	_		-> "(" % var % " :: " % kind % ")"

-----
instance Pretty Fetter where
 prettyp f
  = case f of
  	FConstraint c ts		-> c % " " % " " %!% map prettyTB ts
	FLet        v eff		-> padR 10 (pretty v) % " = " % eff
	
	FProj     pj t1 t2 t3 eff clo	
	 -> "Proj "	% pj	% " " % t1 % " " % t2 % " " % t3 % " " % eff % " " % clo

	FFunInfo v eff   	(TBot _)	-> padR 5 (pretty v) % " = " % eff
	FFunInfo v (TBot _) 	clo		-> padR 5 (pretty v) % " = " % clo
	FFunInfo v eff   	clo		-> padR 5 (pretty v) 	% " = " % eff % "\n"
						%  "        "		% " | " % clo


-----
instance Pretty TypeSource where
 prettyp ts = 
  case ts of
  	TSNil				-> prettyp "@TSNil"
	TSLiteral 	sp c		-> "@TSLiteral " % sp % " " % c
	TSInst   	vDef vInst	-> "@TSInst   "	% vDef % " " % vInst
	TSProj		sp pf		-> "@TSProj "	% sp % pf
	TSLambda 	sp		-> "@TSLambda "	% sp
	TSApp    	sp 		-> "@TSApp    "	% sp

	TSMatchObj 	sp		-> "@TSMatchObj " % sp
	TSMatchAlt 	sp		-> "@TSMatchAlt " % sp
	TSMatch 	sp		-> "@TSMatch "	  % sp
	 
	TSDo		sp		-> "@TSDo "	% sp
	
	TSIfObj		sp		-> "@TSIfObj "	% sp
	TSIfAlt		sp		-> "@TSIfAlt "	% sp
	TSIf		sp		-> "@TSIf "	% sp

	TSStmt		sp		-> "@TSStmt "	% sp
	TSGuard		sp		-> "@TSGuard "	% sp
	TSSig	    	sp v		-> "@TSSig "	% sp % " " % v
	TSField		varT varC varF	-> "@TSField "	% " " %!% [varT, varC, varF]

	TSClassInst	sp v		-> "@TSClasInst " % sp % " " % v
	TSData		sp 		-> "@TSData "	% sp
	TSProjDict	sp 		-> "@TSData " 	% sp
		
	TSSynth	 	v		-> "@TSSynth  " % " " % v
	TSProjCrushed  	cidT cidP pf	-> "@TSProjCrushed " % cidT % " " % cidP % " " % pf
	TSClassName			-> prettyp "@TSClassName "
	
	_ -> panic stage
		$ "pretty[TypeSource]: no match for " % show ts

-----
instance Pretty Kind where
 pretty	k
  = case k of
	KFun k1 k2	-> pretty k1 ++ " -> " ++ pretty k2
	KData		-> "*"
	KRegion		-> "%"
	KEffect		-> "!"
	KFetter		-> "+"
	KClosure	-> "$"

instance  (Pretty param, Pretty t) 
	=> Pretty (InstanceInfo param t) where
 prettyp ii
  = case ii of
  	InstanceLambda v1 v2 mt
	 -> "InstanceLambda " 	% v1 % " " % v2 % " " % mt

	InstanceLet    v1 v2 ts	t 
	 -> "InstanceLet " 	% v1 % " " % v2 % " " % ts % " (" % t % ")"

	InstanceLetRec v1 v2 mt
	 -> "InstanceLetRec "	% v1 % " " % v2 % " " % mt

-----------------------
-- 
prettyTypeSplit :: Type	-> PrettyP
prettyTypeSplit	   x
 = case x of
 	TForall vs t
	 -> "forall " % (" " %!% (map prettyVK vs)) % "\n"
	 %  ".  " % prettyTypeSplit2 t
	 
	t -> prettyTypeSplit2 t
	 
prettyTypeSplit2 x
 = case x of
 	TFetters fs t
	 -> t 	% "\n"
	 % ":- " % prettyTypeFS fs
	
	_ -> prettyp x
	 
prettyTypeFS fs
 	= "\n,  " %!% fs
		

prettyTS t
	= prettyTypeSplit t 



