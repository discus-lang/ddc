{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Pretty printer for type expressions.
module DDC.Type.Pretty
	( prettyTypeParens
	, prettyTypeSplit
	, prettyType
	, prettyKind)
where

-- TODO: move the pretty instance for this elsewhere
import DDC.Solve.Interface.Solution

import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Kind
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Util

stage	= "DDC.Type.Pretty"

-- Bind -------------------------------------------------------------------------------------------
instance Pretty Bind PMode where
  ppr = pprBind Set.empty

pprBind :: Set Var -> Bind -> Str
pprBind vsLocal bb
  = case bb of
	BNil		-> ppr "_"
  	BVar v		-> pprVar vsLocal v
	BMore v t	-> "(" % pprVar vsLocal v % " :> " % pprType vsLocal t % ")"


-- Type --------------------------------------------------------------------------------------------
instance Pretty Type PMode where
  ppr = pprType Set.empty

prettyType :: Type -> Str
prettyType = pprType Set.empty

-- | When pretty printing type we don't want to show module identifiers on locally quantified vars. 
--   We keep a set of which vars are quantified as we decend into the type.
--   TODO: the handling of quant vars not done yet.
pprType :: Set Var -> Type -> Str
pprType vsLocal tt
 = let down = pprType vsLocal 
   in  case tt of
 	TNil		-> ppr "@TNil"

	TForall BNil k t 
	 -> k % " => " % down t

	TForall b _ _	
	 -> let	(bks, tBody) 	= takeTForall tt
		Just v		= takeVarOfBind b
		vsLocal'	= Set.insert v vsLocal
	    in	"forall " 
			% punc " " (map (uncurry pprBindKind) bks) 
			% ". " 
			% pprType vsLocal' tBody

	TConstrain t (Constraints { crsEq, crsMore, crsOther })
	 -> let down' x = case x of
				TForall{}	-> parens $ down x
				_		-> down x

	    in down' t % " :- " 
		% ", " %!% [down t1 % " =  " % down t2 | (t1, t2) <- Map.toList crsEq ]
		% ", " %!% [down t1 % " :> " % down t2 | (t1, t2) <- Map.toList crsMore ]
		% ", " %!% (map (pprFetter vsLocal) crsOther)
		
	TSum k  es	
	 -> k  % "{" % "; " %!% map down es % "}"

	-- type elaboration in source types
	TApp (TCon (TyConElaborate elab _)) t
	 -> pprTypeParens vsLocal t % "{" % elab % "}"

	TApp (TCon (TyConClosure (TyConClosureFree v) _)) t
	 -> v % " : " % pprType vsLocal t

	TApp (TCon (TyConClosure (TyConClosureFreeType v) _)) t
	 -> v % " : " % pprType vsLocal t

	TApp (TCon (TyConClosure (TyConClosureFreeRegion v) _)) t
	 -> v % " : " % pprType vsLocal t

	TApp (TApp (TCon (TyConClosure TyConClosureDanger _)) v) t
	 -> v % " $> " % pprType vsLocal t

	TApp t1 t2
 	 | Just (t1', t2', eff, clo)	<- takeTFun tt
	 -> let str
	 	 | eff == tPure
		 , clo == tEmpty
		 = pprFunArg vsLocal t1' 
		 	% " -> "
			% pprFunResult vsLocal t2'
				
		 | clo == tEmpty
		 = pprFunArg vsLocal t1' 
			% " -(" % pprType vsLocal eff % ")> " 
			% pprFunResult vsLocal t2'
				
		 | eff == tPure
		 = pprFunArg vsLocal t1'
			% " -(" % pprType vsLocal clo % ")> "
			% pprFunResult vsLocal t2'
				
		 | otherwise
		 = pprFunArg vsLocal t1'
			% " -(" % pprTypeParens vsLocal eff 
				% " " 
				% pprTypeParens vsLocal clo % ")> "
			% pprFunResult vsLocal t2'
	    in str

	 | otherwise
	 -> let	pprAppLeft x 
	 	  | isTApp x 	 = pprType vsLocal x
		  | otherwise	 = pprTypeParens vsLocal x

		pprAppRight x
		  | isSomeTVar x = pprType vsLocal x
		  | otherwise	 = pprTypeParens vsLocal x

	    in  pprAppLeft t1 % " " % pprAppRight t2

	TCon tycon	
	 -> pprTyCon vsLocal tycon

	TVar k (UVar v)	   -> pprVarKind vsLocal v k 
	TVar k (UClass c)  -> resultKind k % c
	TVar k (UIndex ix) -> resultKind k % ix

	TVar k (UMore v t)
	 -> ifMode (elem PrettyCoreMore)
	 	(if k == kValue 
			then	parens $ "*" % v % " :> " % pprType vsLocal t 
			else	parens $       v % " :> " % pprType vsLocal t)
			
		(if k == kValue
			then	"*" % v
			else	      ppr v)

	TError k _	-> "@TError" % k % "..."


-- | Pretty print a type that appears on the left of a function arrow.
pprFunArg vsLocal t
	| Just{}	<- takeTFun t
	= parens (pprType vsLocal t)

	| TForall{}	<- t
	= parens (pprType vsLocal t)

	| otherwise
	= pprType vsLocal t


-- | Pretty print a type that appears on the right of a function arrow.
pprFunResult vsLocal tt
 	= pprType vsLocal tt


-- | Pretty print a variable with its kind.
pprVarKind :: Set Var -> Var -> Kind -> PrettyM PMode
pprVarKind vsLocal v k
 = ifMode 
 	(elem PrettyTypeKinds)
	(if kindOfSpace (varNameSpace v) == Just k
		then pprVar vsLocal v
		else "(" % pprVar vsLocal v % " :: " % k % ")")

	(pprVar vsLocal v)


-- | Pretty print a binder with its kind.
pprBindKind :: Bind -> Kind -> PrettyM PMode
pprBindKind bb k
 = ifMode 
	(\mode -> elem PrettyTypeKinds mode || interesting)
	(pprBindWithKind True bb k)
	(pprBindWithKind False bb k)

 where	interesting
	 | Just v	<- takeVarOfBind bb
	 = kindOfSpace (varNameSpace v) /= Just k
	
	 | otherwise
	 = False
	

pprBindWithKind :: Bool -> Bind -> Kind -> PrettyM PMode
pprBindWithKind withKind bb k 
 = case bb of
	BNil		-> "_ :: " % k
 	BVar v
	 | withKind	-> parens $ v %% "::" %% k
	 | otherwise	-> ppr v
	
	BMore v t
	 | withKind	-> parens $ v %% "::" %% k %% ":>" %% t
	 | otherwise	-> parens $ v %% ":>" %% t
	
	

-- | Pretty print a type, wrapping it in parens if it's not some atomic thing like a `TVar` or `TSum`.
pprTypeParens :: Set Var -> Type -> Str
pprTypeParens vsLocal t
 = case t of
	TVar{}	 	-> pprType vsLocal t
	TSum{}		-> pprType vsLocal t
	TCon{}		-> pprType vsLocal t
	_		-> "(" % pprType vsLocal t % ")"


prettyTypeParens = pprTypeParens Set.empty


-- | Pretty print a type with the fetters on their own lines.
prettyTypeSplit :: Type	-> PrettyM PMode
prettyTypeSplit	tt
 = case tt of
 	TForall{}
	 -> let (bks, tBody)	= takeTForall tt
		vsLocal		= Set.fromList
				$ catMaybes 
				$ map (takeVarOfBind . fst) bks
	    in	"forall " 
			% punc " " (map (uncurry pprBindKind) bks) % "\n"
	    		% ".  " 
			% prettyTypeSplit_crs vsLocal tBody
	 
	t	-> prettyTypeSplit_crs Set.empty t


prettyTypeSplit_crs :: Set Var -> Type -> Str
prettyTypeSplit_crs vsLocal xx
 = case xx of
	TConstrain tBody crs
	 -> let	vsBound	= Set.union
				(Set.fromList $ mapMaybe takeVar $ Map.keys $ crsEq crs)
				(Set.fromList $ mapMaybe takeVar $ Map.keys $ crsMore crs)
		
		takeVar  t
			= case t of
				TVar _ u -> takeVarOfBound u
				_	 -> Nothing
		
		vsLocal' = Set.union vsLocal vsBound
		
		down x = case x of
				TForall{}	-> parens $ pprType vsLocal' x
				TConstrain{}	-> parens $ pprType vsLocal' x
				_		-> pprType vsLocal' x
	
	    in	down tBody % "\n"
	 	 % ":- "
	 	 % (punc (ppr "\n,  ") $ 
			(  [pprType vsLocal' t1 %> " =  " % pprType vsLocal' t2	
				| (t1, t2) <- Map.toList $ crsEq crs]
				
			++ [pprType vsLocal' t1 %> " :> " % pprType vsLocal' t2
				| (t1, t2) <- Map.toList $ crsMore crs]

			++ [pprFetter vsLocal' f 
				| f        <- crsOther crs]))
		
	_ -> pprType vsLocal xx


-- | Pretty print a var, but suppress the module if it's in the given set.
pprVar :: Set Var -> Var -> Str
pprVar vsLocal v
	| varModuleId v == ModuleId ["Base"]
	= ppr $ varWithoutModuleId v

 	| Set.member v vsLocal
	= ppr $ varWithoutModuleId v

	| otherwise
	= ppr v
	

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
  ppr = pprFetter Set.empty

pprFetter vsLocal ff
  = case ff of
  	FConstraint c ts
 	 -> pprVar vsLocal c  % " " % " " %!% map (pprTypeParens vsLocal) ts

	FWhere t1 t2
	 -> padL 10 (pprType vsLocal t1) 
		% " = "
		% (pprType vsLocal t2)

	FMore t1 t2
	 -> padL 10 (pprType vsLocal t1)
		% " :> "
		% (pprType vsLocal t2)

	FProj     pj v1 tDict tBind
	 -> "Proj "
		% pj
		% " " % v1
		% " " % pprType vsLocal tDict 
		% " " % pprType vsLocal tBind


-- Super -------------------------------------------------------------------------------------------
instance Pretty Super PMode where
 ppr k
  = case k of
	SProp		-> ppr "+"
	SBox		-> ppr"[]"
	SFun k1 s2	-> prettyKindLeft k1 % " -> " % s2


-- Kind --------------------------------------------------------------------------------------------
prettyKind :: Kind -> Str
prettyKind = ppr

instance Pretty Kind PMode where
 ppr kk
  = case kk of
	KNil		-> ppr "?"
	KCon k _	-> ppr k
	KFun k1 k2	-> prettyKindLeft k1 % " -> " % k2
	KApp k1 t1	-> k1 % " " % pprTypeParens Set.empty t1

	KSum [k]	-> ppr k
	KSum ks		-> "+" % (braces $ punc ", " ks)

prettyKindLeft kk
 = case kk of 
	KFun{}		-> parens $ ppr kk
	KApp{}		-> parens $ ppr kk
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
			%>  (prettyTypeSplit t)
		% "\n\n"

	InstanceLetRec v1 v2 mt
	 -> "InstanceLetRec "	% v1 % " " % v2 % " " % mt


-- TyCon ------------------------------------------------------------------------------------------
instance Pretty TyCon PMode where
 ppr = pprTyCon Set.empty

pprTyCon vsLocal p
  = case p of
  	TyConFun{}		
	 -> ppr "(->)"

	TyConData { tyConName, tyConDataKind }	
	  -> ifMode (elem PrettyTypeKinds)
 		(parens $ tyConName 
			% " :: " 
			% tyConDataKind)
		(pprVar vsLocal tyConName)
	
	TyConEffect { tyConEffect, tyConEffectKind }
	  -> ifMode (elem PrettyTypeKinds)
 		(parens $ tyConEffect 
			% " :: " 
			% tyConEffectKind)
		(ppr tyConEffect)

	TyConClosure { tyConClosure, tyConClosureKind }
	  -> ifMode (elem PrettyTypeKinds)
 		(parens $ tyConClosure
			% " :: " 
			% tyConClosureKind)
		(ppr tyConClosure)

	TyConWitness { tyConWitness, tyConWitnessKind}	
	 -> ifMode (elem PrettyTypeKinds)
 		(parens $ ppr tyConWitness 
			% " :: " 
			% tyConWitnessKind)
		(ppr tyConWitness)

	TyConElaborate { tyConElaborate, tyConElaborateKind}	
	 -> ifMode (elem PrettyTypeKinds)
 		(parens $ ppr tyConElaborate 
			% " :: " 
			% tyConElaborateKind)
		(ppr tyConElaborate)


-- TyConEffect ------------------------------------------------------------------------------------
instance Pretty TyConEffect PMode where
 ppr cc
  = case cc of
	TyConEffectTop var		-> pprVar Set.empty  var
	TyConEffectRead			-> ppr "!Read"
	TyConEffectHeadRead		-> ppr "!ReadH"
	TyConEffectDeepRead		-> ppr "!ReadT"
	TyConEffectWrite		-> ppr "!Write"
	TyConEffectDeepWrite		-> ppr "!WriteT"


-- TyConClosure -----------------------------------------------------------------------------------
instance Pretty TyConClosure PMode where
 ppr cc
  = case cc of
	TyConClosureFree var		-> ppr "$FreeClosure_" % var
	TyConClosureFreeType var	-> ppr "$FreeType_" % var
	TyConClosureFreeRegion var	-> ppr "$FreeRegion_" % var
	TyConClosureDanger		-> ppr "$Danger"


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


-- TyConElaborate --------------------------------------------------------------------------------
instance Pretty TyConElaborate PMode where
 ppr cc 
  = case cc of
	TyConElaborateRead		-> ppr "read"
	TyConElaborateWrite		-> ppr "write"
	TyConElaborateModify		-> ppr "modify"


-- KiCon ------------------------------------------------------------------------------------------
instance Pretty KiCon PMode where
 ppr con
  = case con of
	KiConVar v			-> ppr v
	KiConBox			-> ppr "[]"
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

