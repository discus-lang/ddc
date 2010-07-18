{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Pretty printer for type expressions.
module DDC.Type.Pretty
	( prettyTypeParens
	, prettyTypeSplit
	, prettyType
	, prettyKind)
where
import DDC.Solve.InstanceInfo
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
 ppr xx
  = case xx of
	BNil		-> ppr "_"
  	BVar v		-> ppr v
	BMore v t	-> "(" % v % " :> " % t % ")"

-- Type --------------------------------------------------------------------------------------------
prettyType :: Type -> Str
prettyType = ppr

instance Pretty Type PMode where
 ppr tt = pprTypeQuant Set.empty tt

-- | When pretty printing type we don't want to show module identifiers on locally quantified vars. 
--   We keep a set of which vars are quantified as we decend into the type.
--   TODO: the handling of quant vars not done yet.
pprTypeQuant :: Set Var -> Type -> Str
pprTypeQuant vsQuant tt
 = let down = pprTypeQuant vsQuant 
   in  case tt of
 	TNil		-> ppr "@TNil"

	TForall BNil k t 
	 -> k % " => " % down t

	TForall b _ _	
	 -> let	(bks, tBody) 	= takeTForall tt
		Just v		= takeVarOfBind b
		vsQuant'	= Set.insert v vsQuant
	    in	"forall " 
			% punc " " (map (uncurry pprBindKind) bks) 
			% ". " 
			% pprTypeQuant vsQuant' tBody

	TFetters t fs	-> down t % " :- " % ", " %!% fs
	
	TConstrain t (Constraints { crsEq, crsMore, crsOther })
	 -> let down' x = case x of
				TForall{}	-> parens $ ppr x
				_		-> ppr x

	    in down' t % " :- " 
		% ", " %!% [down t1 % " =  " % down t2 | (t1, t2) <- Map.toList crsEq ]
		% ", " %!% [down t1 % " :> " % down t2 | (t1, t2) <- Map.toList crsMore ]
		% ", " %!% crsOther
		
	TSum k  es	-> k  % "{" % "; " %!% map down es % "}"

	-- type elaboration in source types
	TApp (TCon (TyConElaborate elab _)) t
	 -> prettyTypeParens t % "{" % elab % "}"

	TApp (TCon (TyConClosure (TyConClosureFree v) _)) t
	 -> v % " : " % t

	TApp (TCon (TyConClosure (TyConClosureFreeType v) _)) t
	 -> v % " : " % t

	TApp (TCon (TyConClosure (TyConClosureFreeRegion v) _)) t
	 -> v % " : " % t

	TApp (TApp (TCon (TyConClosure TyConClosureDanger _)) v) t
	 -> v % " $> " % t

	TApp t1 t2
 	 | Just (t1', t2', eff, clo)	<- takeTFun tt
	 -> let str
	 	 | eff == tPure
		 , clo == tEmpty
		 = prettyTypeLeft t1' % " -> " % prettyTypeRight t2'
				
		 | clo == tEmpty
		 = prettyTypeLeft t1' % " -(" % eff % ")> " % prettyTypeRight t2'
				
		 | eff == tPure
		 = prettyTypeLeft t1' % " -(" % clo % ")> " % prettyTypeRight t2'
				
		 | otherwise
		 = prettyTypeLeft t1' % " -(" % prettyTypeParens eff % " " % prettyTypeParens clo % ")> " % prettyTypeRight t2'
	    in str

	 | otherwise
	 -> let	pprAppLeft x 
	 	  | isTApp x 	= ppr x
		  | otherwise	= prettyTypeParens x

		pprAppRight x
		  | isSomeTVar x 
		  = ppr x

		  | otherwise		
		  = prettyTypeParens x

	    in  pprAppLeft t1 % " " % pprAppRight t2

	TCon tycon	-> ppr tycon

	TVar k (UVar v)	   -> pprVarKind v k 
	TVar k (UClass c)  -> resultKind k % c
	TVar k (UIndex ix) -> resultKind k % ix

	TVar k (UMore v t)
	 -> ifMode (elem PrettyCoreMore)
	 	(if k == kValue 
			then	parens $ "*" % v % " :> " % t 
			else	parens $ v % " :> " % t)
			
		(if k == kValue
			then	"*" % v
			else	ppr v)

	TError k _	-> "@TError" % k % "..."


-- | Pretty print a type that appears on the right of a function arrow.
prettyTypeRight tt
 = case tt of
 	TFetters{}	-> "(" % tt % ")"
	_		-> ppr tt


-- | Pretty print a type that appears on the left of a function arrow.
prettyTypeLeft t
	| Just{}	<- takeTFun t
	=  "(" % t % ")"

	| otherwise
	= ppr t


-- | Pretty print a variable with its kind.
pprVarKind :: Var -> Kind -> PrettyM PMode
pprVarKind v k
 = ifMode 
 	(elem PrettyTypeKinds)
	(if kindOfSpace (varNameSpace v) == Just k
		then ppr v
		else "(" % ppr v % " :: " % k % ")")

	(ppr v)


-- | Pretty print a binder with its kind.
pprBindKind :: Bind -> Kind -> PrettyM PMode
pprBindKind bb k
 = case bb of
	BNil		-> "_ :: " % k
 	BVar v		-> pprVarKind (varWithoutModuleId v) k
	BMore v t	-> pprVarKind (varWithoutModuleId v) k % " :> " % t
	

-- | Pretty print a type, wrapping it in parens if it's not some atomic thing like a `TVar` or `TSum`.
prettyTypeParens :: Type -> Str
prettyTypeParens t
 = case t of
	TVar{}	 	-> ppr t
	TSum{}		-> ppr t
	TCon{}		-> ppr t
	_		-> "(" % t % ")"

-- | Pretty print a type with the fetters on their own lines.
prettyTypeSplit :: Type	-> PrettyM PMode
prettyTypeSplit	   tt
 = case tt of
	TForall BNil _ _	-> prettyTypeSplit_crs tt

 	TForall{}
	 -> let (bks, tBody)	= takeTForall tt
	    in	"forall " 
			% punc " " (map (uncurry pprBindKind) bks) % "\n"
	    		% ".  " % prettyTypeSplit_crs tBody
	 
	t -> prettyTypeSplit_crs t

prettyTypeSplit_crs :: Type -> Str
prettyTypeSplit_crs xx
 = let down x = case x of
			TForall{}	-> parens $ ppr x
			TConstrain{}	-> parens $ ppr x
			TFetters{}	-> parens $ ppr x
			_		-> ppr x
   in case xx of
	TConstrain t crs
	 -> down t % "\n"
	 % ":- "
	 % (punc (ppr "\n,  ") $ 
		(  [t1 %> " =  " % t2	| (t1, t2) <- Map.toList $ crsEq crs]
		++ [t1 %> " :> " % t2	| (t1, t2) <- Map.toList $ crsMore crs]
		++ [ppr f 		| f        <- crsOther crs]))
		
 	TFetters t fs
	 -> down t 	% "\n" 
	 % ":- " 
	 % punc (ppr "\n,  ") fs

	_ -> ppr xx


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
  	FConstraint	c ts	-> c  % " " % " " %!% map prettyTypeParens ts
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
	KApp k1 t1	-> k1 % " " % prettyTypeParens t1

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
	TyConEffectTop var		-> ppr var
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

