{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Pretty printer for type expressions.
module Type.Pretty
	( prettyTB
	, prettyTypeSplit
	, prettyTS)
where
import Type.Exp
import Type.Util.Bits
import Util
import Type.Builtin
import DDC.Solve.InstanceInfo
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
-- import Data.Set			(Set)
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Type.Pretty"

-- Bind -------------------------------------------------------------------------------------------
instance Pretty Bind PMode where
 ppr xx
  = case xx of
  	BVar v		-> ppr v
	BMore v t	-> "(" % v % " :> " % t % ")"

-- Type --------------------------------------------------------------------------------------------
instance Pretty Type PMode where
 ppr tt = pprTypeQuant Set.empty tt


-- | When pretty printing type we don't want to show module identifiers on quantified vars. 
--	We keep a set of which vars are quantified as we decend into the type.
--	TODO: handling of quant vars not done yet.
pprTypeQuant :: Set Var -> Type -> Str
pprTypeQuant vsQuant tt
 = let down = pprTypeQuant vsQuant 
   in  case tt of
 	TNil		-> ppr "@TNil"

	TForall b k t	
	 -> let	(bks, tBody) 	= slurpTForall tt
		vsQuant'	= Set.insert (varOfBind b) vsQuant
	    in	"forall " 
			% punc " " (map (uncurry pprBindKind) bks) 
			% ". " 
			% pprTypeQuant vsQuant' tBody

	TContext c t	-> c % " => " % down t

	TFetters t fs	-> down t % " :- " % ", " %!% fs
	
	TConstrain t (Constraints { crsEq, crsMore, crsOther })
	 -> down t % " :- " 
		% ", " %!% [down t1 % " =  " % down t2 | (t1, t2) <- Map.toList crsEq ]
		% ", " %!% [down t1 % " :> " % down t2 | (t1, t2) <- Map.toList crsMore ]
		% ", " %!% crsOther
		
	TSum k  es	-> k  % "{" % "; " %!% map down es % "}"

	TApp t1 t2
	 -> let result
	 		| Just (t1, t2, eff, clo)	<- takeTFun tt
			= let str
				| eff == tPure
				, clo == tEmpty
				= prettyTBF t1 % " -> " % prettyTRight t2
				
				| clo == tEmpty
				= prettyTBF t1 % " -(" % eff % ")> " % prettyTRight t2
				
				| eff == tPure
				= prettyTBF t1 % " -(" % clo % ")> " % prettyTRight t2
				
				| otherwise
				= prettyTBF t1 % " -(" % prettyTB eff % " " % prettyTB clo % ")> " % prettyTRight t2
			   in	str

			| otherwise
			= let	pprAppLeft x 
			 	  | isTApp x 	= ppr x
				  | otherwise	= prettyTB x

				pprAppRight x
				  | isSomeTVar x 
				  = ppr x

				  | otherwise		
				  = prettyTB x

			 in	pprAppLeft t1 % " " % pprAppRight t2
	   in result


	TCon tycon	-> ppr tycon

	TVar k v	-> pprVarKind v k 

	TTop k		-> prettyKB k % "Top"
	TBot k		-> prettyKB k % "Bot"
		
	-- effect
	TEffect    v []	-> ppr v
	TEffect    v ts	-> v % " " % " " %!% map prettyTB ts

	-- closure
	TFree  v t	-> v % " : " % t
	TDanger v t	-> v % " $> " % t
	
	-- used in type inference
	TClass k c	-> resultKind k % c
	TError k t	-> "@TError" % k % "..."

	-- type elaboration in source
	TElaborate elab t -> prettyTB t % "{" % elab % "}"

	-- core stuff
	TVarMore k v t
	 -> ifMode (elem PrettyCoreMore)
	 	(if k == kValue 
			then	parens $ "*" % v % " :> " % t 
			else	parens $ v % " :> " % t)
			
		(if k == kValue
			then	"*" % v
			else	ppr v)

	TIndex k ix
	 -> ppr (resultKind k) % ix

prettyTRight tt
 = case tt of
 	TFetters{}	-> "(" % tt % ")"
	_		-> ppr tt


prettyTBF t
	| Just{}	<- takeTFun t
	=  "(" % t % ")"

	| otherwise
	= ppr t


prettyTB t
 = case t of
	TVar k v 	-> ppr t
	TSum{}		-> ppr t
	TEffect v []	-> ppr t
	TClass{}	-> ppr t
	TBot{}		-> ppr t
	TTop{}		-> ppr t
	TCon{}		-> ppr t
	_		-> "(" % t % ")"


pprVarKind :: Var -> Kind -> PrettyM PMode
pprVarKind v k
 = ifMode 
 	(elem PrettyTypeKinds)
	(if kindOfSpace (varNameSpace v) == Just k
		then ppr v
		else "(" % ppr v % " :: " % k % ")")

	(ppr v)

pprBindKind :: Bind -> Kind -> PrettyM PMode
pprBindKind bb k
 = case bb of
 	BVar v		-> pprVarKind (varWithoutModuleId v) k
	BMore v t	-> pprVarKind (varWithoutModuleId v) k % " :> " % t
	

-- | Get the kind associated with a namespace.
--	This is a local local copy to avoid module recursion.
--	Also in Type.Util.Bits
kindOfSpace :: NameSpace -> Maybe Kind
kindOfSpace space
 = case space of
 	NameType	-> Just kValue
	NameRegion	-> Just kRegion
	NameEffect	-> Just kEffect
	NameClosure	-> Just kClosure
	_		-> Nothing

-- | Get the result of applying all the paramters to a kind.
resultKind :: Kind -> Kind
resultKind kk
 = case kk of
 	KFun k1 k2	-> resultKind k2
	_		-> kk


-- | Prints a type with the fetters on their own lines
prettyTypeSplit :: Type	-> PrettyM PMode
prettyTypeSplit	   tt
 = case tt of
 	TForall b k t
	 -> let (bks, tBody)	= slurpTForall tt
	    in	"forall " % punc " " (map (uncurry pprBindKind) bks) % "\n"
	    		% ".  " % prettyTypeSplit2 tBody
	 
	t -> prettyTypeSplit2 t
	 
prettyTypeSplit2 x
 = case x of
 	TFetters t fs
	 -> t 	% "\n"
	 % ":- " % prettyTypeFS fs
	
	_ -> ppr x
	 
prettyTypeFS fs
 	= "\n,  " %!% fs
		
prettyTS t
	= prettyTypeSplit 
	$ toFetterFormT t 



-- Elaboration -------------------------------------------------------------------------------------
instance Pretty Elaboration PMode where
 ppr ee
  = case ee of
	ElabRead	-> ppr "read"
	ElabWrite	-> ppr "write"
	ElabModify	-> ppr "modify"
	


-- TProj -------------------------------------------------------------------------------------------
instance Pretty TProj PMode where
 ppr p
  = case p of
  	TJField  v	-> "." % varWithoutModuleId v
	TJFieldR v	-> "#" % varWithoutModuleId v
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
	FWhere		t1 t2	-> padL 10 t1 % " = "	% t2
	FMore		t1 t2	-> padL 10 t1 % " :> " % t2

	FProj     pj v1 tDict tBind
	 -> "Proj "	% pj	% " " % v1 % " " % tDict % " " % tBind


-- Super -------------------------------------------------------------------------------------------
instance Pretty Super PMode where
 ppr k
  = case k of
	SProp		-> ppr "+"
	SBox		-> ppr"[]"
	SFun k1 s2	-> k1 % " -> " % s2


-- Kind --------------------------------------------------------------------------------------------
instance Pretty Kind PMode where
 ppr k
  = case k of
	KNil		-> ppr "?"
	KCon k s	-> ppr k
	KFun k1 k2	-> k1 % " -> " % k2
	KApp k1 t1	-> k1 % " " % prettyTB t1

	KSum [k]	-> ppr k
	KSum ks		-> "+" % (braces $ punc ", " ks)


-- | Pretty print a kind, wrapping funs in parens
prettyKB :: Kind -> PrettyM PMode
prettyKB kk
 = case kk of
	KFun{}		-> "(" % kk % ")"
	KApp{}		-> "(" % kk % ")"
	_		-> ppr kk

	
-- InstanceInfo ------------------------------------------------------------------------------------
instance  (Pretty param PMode)
	=> Pretty (InstanceInfo param) PMode where
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


-- TyCon ------------------------------------------------------------------------------------------
instance Pretty TyCon PMode where
 ppr p
  = case p of
  	TyConFun{}		
	 -> ppr "(->)"

	TyConData { tyConName, tyConDataKind }	
	  -> ifMode (elem PrettyTypeKinds)
 		(parens $ tyConName 
			% " :: " 
			% tyConDataKind)
		(ppr tyConName)

	TyConWitness { tyConWitness, tyConWitnessKind}	
	 -> ifMode (elem PrettyTypeKinds)
 		(parens $ ppr tyConWitness 
			% " :: " 
			% tyConWitnessKind)
		(ppr tyConWitness)


-- TyConWitness -----------------------------------------------------------------------------------
instance Pretty TyConWitness PMode where
 ppr cc
  = case cc of
	TyConWitnessMkVar var		-> varModuleId var % "." % "Mk" % varName var
  	TyConWitnessMkConst		-> ppr "MkConst"
	TyConWitnessMkDeepConst		-> ppr "MkDeepConst"
	TyConWitnessMkMutable		-> ppr "MkMutable"
	TyConWitnessMkDeepMutable	-> ppr "MkDeepMutable"
	TyConWitnessMkLazy		-> ppr "MkLazy"
	TyConWitnessMkHeadLazy		-> ppr "MkHeadLazy"
	TyConWitnessMkDirect		-> ppr "MkDirect"
	TyConWitnessMkPurify		-> ppr "MkPurify"
	TyConWitnessMkPure		-> ppr "MkPure"
	TyConWitnessMkEmpty		-> ppr "MkEmpty"


-- KiCon ------------------------------------------------------------------------------------------
instance Pretty KiCon PMode where
 ppr con
  = case con of
	KiConVar v			-> ppr v
	KiConValue			-> ppr "*"
	KiConRegion			-> ppr "%"
	KiConEffect			-> ppr "!"
	KiConClosure			-> ppr "$"
	KiConMutable			-> ppr "Mutable"
	KiConDeepMutable		-> ppr "DeepMutable"
	KiConConst			-> ppr "Const"
	KiConDeepConst			-> ppr "DeepConst"
	KiConLazy			-> ppr "Lazy"
	KiConHeadLazy			-> ppr "HeadLazy"
	KiConDirect			-> ppr "Direct"
	KiConPure			-> ppr "Pure"
	KiConEmpty			-> ppr "Empty"

