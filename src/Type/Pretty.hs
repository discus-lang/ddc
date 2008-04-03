{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Pretty printer for type expressions.
module Type.Pretty
	( prettyTB
	, prettyVK
	, prettyTypeSplit
	, prettyTS)

where

import Type.Exp
import Shared.Pretty
import Shared.Error
import Util

-----
stage	= "Type.Pretty"

-- Type --------------------------------------------------------------------------------------------
instance Pretty Type PMode where
 ppr xx
  = case xx of
 	TNil		-> ppr "@TNil"

	TForall vs t	-> "forall " % " " %!% (map prettyVK vs) % ". " % t
	TFetters fs t	-> t % " :- " % ", " %!% fs
	TSum	k  es	-> k  % "{" % "; " %!% es % "}"
	TMask	k  t1 t2 -> prettyTB t1 % " \\ " % prettyTB t2
	TApp	t1 t2	 -> prettyTBF t1 % " " % prettyTRight t2
	TCon	tycon	-> ppr tycon
	TVar k v	-> ppr v

	TTop k		-> k % "Top"
	TBot k		-> k % "Bot"


	-- data
	TData v []	-> ppr v 
	TData v ts	-> v % " " % " " %!% (map prettyTB ts)

	TFun t1 t2 eff clo
	 -> case (eff, clo) of
	 	(TBot _ ,	TBot _)	-> prettyTBF t1 % " -> " % prettyTRight t2
		(eff',		TBot _)	-> prettyTBF t1 % " -(" % eff' % ")> " % prettyTRight t2
		(TBot _,	clo')	-> prettyTBF t1 % " -(" % clo' % ")> " % prettyTRight t2
		(eff',		clo')	-> prettyTBF t1 % " -(" % prettyTB eff' % " " % prettyTB clo' % ")> " 
								% prettyTRight t2
		
	-- effect
	TEffect    v []	-> ppr v
	TEffect    v ts	-> v % " " % " " %!% map prettyTB ts

	-- closure
	TFree  v t	-> v % " : " % t
	TDanger v t	-> v % " $> " % t
	TTag v		-> ppr v
	
	-- wild cards
	TWild k		-> k % "_"


	---- used in the type solver
	TClass k c	-> k % c
	TFetter f	-> "@TFetter " % f
	TError k t	-> "@TError" % k % " " % t

	-----
	TElaborate t	-> "elaborate " % t
	TMutable   t	-> "mutable " 	% t 


prettyTRight tt
 = case tt of
 	TFetters{}	-> "(" % tt % ")"
	_		-> ppr tt


prettyTBF t
 = case t of
 	TFun{}		-> "(" % t % ")"
	TMutable{}	-> "(" % t % ")"
	_ 		-> ppr t

prettyTB t
 = case t of
 	TData v []	-> ppr t
	TVar k v 	-> ppr t
	TSum{}		-> ppr t
	TEffect v []	-> ppr t
	TTag v		-> ppr t
	TWild{}		-> ppr t
	TClass{}	-> ppr t
	TBot{}		-> ppr t
	TTop{}		-> ppr t
	_		-> "(" % t % ")"


-- | Prints a type with the fetters on their own lines
prettyTypeSplit :: Type	-> PrettyM PMode
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
	
	_ -> ppr x
	 
prettyTypeFS fs
 	= "\n,  " %!% fs
		
prettyTS t
	= prettyTypeSplit t 


-- | Prints a variable with an optional kind.
prettyVK ::	(Var, Kind)	-> PrettyM PMode
prettyVK	(var, kind)
 = case kind of
	KData		-> ppr var
	KRegion		-> ppr var
	KEffect		-> ppr var
	KClosure	-> ppr var
	_		-> "(" % var % " :: " % kind % ")"

-- TyCon -------------------------------------------------------------------------------------------
instance Pretty TyCon PMode where
 ppr p
  = case p of
  	TyConFun{}		-> ppr "(->)"
	TyConData { tyConName }	-> ppr tyConName


-- TProj -------------------------------------------------------------------------------------------
instance Pretty TProj PMode where
 ppr p
  = case p of
  	TJField  v	-> "." % v
	TJFieldR v	-> "#" % v
	_		-> panic stage "ppr[TProj]: no match"


-- ClassId -----------------------------------------------------------------------------------------
instance Pretty ClassId PMode where
 ppr c
  = case c of
  	ClassId i	-> ppr i

-- Fetter ------------------------------------------------------------------------------------------
instance Pretty Fetter PMode where
 ppr f
  = case f of
  	FConstraint	c ts	-> c  % " " % " " %!% map prettyTB ts
	FLet		t1 t2	-> padL 10 t1 % " = "	% t2
	FMore		t1 t2	-> padL 10 t1 % " :> " % t2

	FProj     pj v1 tDict tBind
	 -> "Proj "	% pj	% " " % v1 % " " % tDict % " " % tBind


-- Kind --------------------------------------------------------------------------------------------
instance Pretty Kind PMode where
 ppr k
  = case k of
	KFun k1 k2	-> k1 % " -> " % k2
	KData		-> ppr "*"
	KRegion		-> ppr "%"
	KEffect		-> ppr "!"
	KFetter		-> ppr"+"
	KClosure	-> ppr "$"
	_		-> panic stage "pretty[Kind]: no match"


-- InstanceInfo ------------------------------------------------------------------------------------
instance  (Pretty param PMode)
	=> Pretty (InstanceInfo param Type) PMode where
 ppr ii
  = case ii of
  	InstanceLambda v1 v2 mt
	 -> "InstanceLambda " 	% v1 % " " % v2 % " " % mt

	InstanceLet    v1 v2 ts	t 
	 -> "InstanceLet\n" 	
	 	% "    use var     = " % v1 % "\n"
		% "    binding var = " % v2 % "\n\n"
		% "    type parameters:\n"
			%> ("\n" %!% ts) % "\n\n"
		% "    scheme:\n"
			%>  (prettyTS t)
		% "\n\n"

	InstanceLetRec v1 v2 mt
	 -> "InstanceLetRec "	% v1 % " " % v2 % " " % mt

