{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -O2 #-}

-- | Pretty printing of Sea expressions.
module DDC.Sea.Pretty
	(seaVar)
where
import DDC.Sea.Exp
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Base.DataFormat
import DDC.Base.SourcePos
import DDC.Base.Literal
import DDC.Var
import qualified Shared.VarUtil	as Var
import qualified Shared.VarPrim	as Var
import qualified Data.Map	as Map
import Data.Function
import Util

stage	= "Sea.Pretty"

-- Show global sea vars
sV  v		= ppr $ seaVar False v
sVn n v		= ppr $ padL n $ seaVar False v

-- Show local sea vars
sVL  v		= ppr $ seaVar True v


-- Top ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Top (Maybe a)) PMode where
 ppr xx
  = case xx of
	PNil	 		-> ppr $ "$PNil\n"

	PData v ctors
	 | Map.null ctors
	 -> "data " % " " % ppr v % ";\n"

	 | otherwise
	 -> let ctorsList = sortBy (compare `on` ctorDefTag) $ Map.elems ctors
	    in  "data" <> v <> "where\n"
	 	% "{\n"
	 	%> ("\n\n" %!% ctorsList % "\n")
		% "}\n"

	-- supers
	PProto v argTypes resultType
	 -> resultType %>> sVn 8 v %>> "(" % ", " %!% argTypes % ");\n"

	PSuper v vts rt ss
	 -> rt %>> sV v %>>"(" % ", " %!% (map (\(av, at) -> at % " " % sVL av) vts) % ")" % "\n"
	  % "{\n"
	  	%> ("\n" %!% ss % "\n")
	  % "}\n\n\n"

	-- cafs
	PCafProto v t
	 | not $ typeIsBoxed t	 -> ppr "\n"
	 | otherwise		-> "extern " % t % " " %>> "*_ddcCAF_" % sV v % ";\n\n"

	PCafSlot  v t
	 | not $ typeIsBoxed t	-> t % " " %>>  "_ddcCAF_" % sV v % " = 0;\n"
	 | otherwise		-> t % " " %>> "*_ddcCAF_" % sV v % " = 0;\n"


	PCafInit v _ ss
	 -> "void " %>> "_ddcInitCAF_" % sV v %>> "()\n"
	 % "{\n"
	 	%> ("\n" %!% ss % "\n")
	 % "}\n\n\n"

	-- Sea hackery.
	PInclude s	-> "#include <" % s % ">\n"
	PIncludeAbs s	-> "#include \"" % s % "\"\n"
	PHackery []	-> ppr "\n"
	PHackery s	-> ppr s
	PComment []	-> ppr "//\n"
	PComment s	-> "// " % s % "\n"
	PBlank		-> ppr "\n"
	PHashDef s1 s2	-> "#define " %  padL 8 s1 %>> " " % s2 % "\n"

	PMain mn ml withHandler
	 ->	"int main (int argc, char** argv)\n"
	  %	"{\n"
	  %	"\t_ddcRuntimeInit(argc, argv);\n\n"
	  %	"\n"
	  %!%	(map (\mname -> "\t" ++ mname ++ "();") ml)
	  %	"\n\n"
	  %	"\t_ddcInitModule_" % mn % "();\n"
	  %	"\n"
          %     (if withHandler
			then "\tControl_Exception_topHandle(_allocThunk((FunPtr) " % mn % "_main, 1, 0));\n"
			else ppr "\tMain_main(Base_Unit());")
	  %	"\n"
	  %	"\t_ddcRuntimeCleanup();\n"
	  %	"\n"
	  %	"\treturn 0;\n"
	  %	"}\n"

-- CtorDef --------------------------------------------------------------------------------------------
instance Pretty CtorDef PMode where
 ppr xx
  = case xx of
  	CtorDef v t arity tag fs
 	 -> v 	% "\n"
		%> 	( ":: " % ppr t % "\n"
			% "with { ARITY  = " % arity	% "\n"
 			% "     , TAG    = " % tag      % "\n"
			% "     , FIELDS = " % fs 	% "}")


-- Stmt --------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Stmt (Maybe a)) PMode where
 ppr xx
  = case xx of
	-- misc
	SBlank 			-> ppr " "
	SComment s		-> "// " % s

	-- stacks
	SAuto	v t		-> (padL 12 $ pprStrPlain $ pprTypeArg t) % " " % sVL v % ";"
	SEnter countS		-> "_ENTER (" % countS % ");"
	SLeave countS		-> "_LEAVE (" % countS % ");"

	-- assignment
	SAssign (XVar n _) _ x2	-> (padL 15 $ pprStrPlain n) 	% " = " % x2 % ";"
	SAssign x1 _ x2		-> (padL 25 $ pprStrPlain x1) 	% " = " % x2 % ";"

	SStmt s			-> ppr s % ";"

	-- control flow
	SReturn x		-> "return " % x % ";"
	SLabel v		-> sV v % ":"
	SGoto v			-> "goto " % sV v % ";"

	SSwitch x aa
	 -> "switch (" % x % ") {\n"
	    % punc "\n" aa
	    % "}"

	SMatch aa
	 -> "match {\n"
	    % punc "\n" aa
	    % "}"

	SIf xExp ssThen
	 -> "if (" % xExp % ") {\n"
		%> punc "\n" ssThen
		% "\n}\n"

	SCaseFail
	 -> ppr "_CASEFAIL;"

-- Alt ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Alt (Maybe a)) PMode where
 ppr xx
  = case xx of
	AAlt gs ss
	 -> "  alt:  "
	 %> ("\n" %!% gs % "\nthen {\n" %> ("\n" %!% ss) % "\n}\n")
	 % "\n\n"

	ASwitch g []
	 -> "  case " % g % ": break;\n"

	ASwitch g [SGoto v]
	 -> "  case " % g % ": goto " % sV v % ";\n"

	ASwitch g ss
	 -> "  case " % g % ": {\n"
		%> ("\n" %!% ss % "\n"
		%  "break;\n")
	  % "  }\n"

	ACaseSusp x l
	 -> "  _CASESUSP (" % x % ", " % "_" % l % ");\n"

	ACaseIndir x l
	 -> "  _CASEINDIR (" % x % ", " % "_" % l % ");\n"

	ACaseDeath (SourcePos (f, l, c))
	 -> let	-- Hack fixup windows file paths.
		f'	= map (\z -> if z == '\\' then '/' else z) f
	    in	ppr "  _CASEDEATH (\"" % f' % "\", " % l % ", " % c % ");\n"
	 

	ADefault [SGoto v]
	 -> "  default: goto " % sV v % ";\n"

	ADefault ss
	 -> "  default: {\n"
	 	%> ("\n" %!% ss % "\n"
		%   "break;\n")
	  % "  }\n"


-- Guard -------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Guard (Maybe a)) PMode where
 ppr gg
  = case gg of
  	GCase _ True ss x1 x2
	 -> "guard {\n"
	 %> ("\n" %!% ss % "\n") % "}\n"
	 %  "compareLazy " % x1 % " with " % x2 % ";\n";

  	GCase _ False ss x1 x2
	 -> "guard {\n"
	 %> ("\n" %!% ss % "\n") % "}\n"
	 %  "compareDirect " % x1 % " with " % x2 % ";\n";


-- Exp ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Exp (Maybe a)) PMode where
 ppr xx
  = case xx of
  	XNil		-> ppr "@XNil"

	-- variables
	XVar name typ	
	 -> ifMode (elem PrettySeaTypes)
		(parens $ ppr name % " :: " % ppr typ)
		(ppr name)

	-- projection
	XTag x
	 -> "_getObjTag(" % x % ")"

	XArgData x@(XVar _ _) i
	 -> "_DARG(" % x % ", " % i % ")"

	XArgThunk x@(XVar _ _) i
	 -> "_TARG(" % x % ", " % i % ")"

	-- literals
	XLit lit	-> ppr lit

	-- Primitives ---------------------------------------------------------
	-- Primitive arithmetic operators.
	XPrim (MOp f) [x1]
	 |  f == OpNeg
	 -> "-(" % x1 % ")"

	XPrim (MOp f) [x1, x2]
	 -> case f of
	 	OpAdd	-> "(" % x1 % " + "	% x2 % ")"
		OpSub	-> "(" % x1 % " - "	% x2 % ")"
		OpMul	-> "(" % x1 % " * "	% x2 % ")"
		OpDiv	-> "(" % x1 % " / "	% x2 % ")"
		OpMod	-> "(" % x1 % " % "	% x2 % ")"
	 	OpEq	-> "(" % x1 % " == "	% x2 % ")"
		OpNeq	-> "(" % x1 % " != "	% x2 % ")"

	 	OpGt	-> "(" % x1 % " > "	% x2 % ")"
	 	OpLt	-> "(" % x1 % " < "	% x2 % ")"
	 	OpGe	-> "(" % x1 % " >= "	% x2 % ")"
	 	OpLe	-> "(" % x1 % " <= "	% x2 % ")"

		OpAnd	-> "(" % x1 % " && "	% x2 % ")"
		OpOr	-> "(" % x1 % " || "	% x2 % ")"
		_	-> panic stage ("ppr[Exp]: no match for " % show xx)

	-- Primitive function application operators.
	XPrim (MApp f) (x : args)
	 |  XVar _ _ <- x
	 -> case f of
		PAppTailCall	
		 -> "@XTailCall " % x % " (" % ", " %!% args % ")"

		PAppCall
		 -> x % " (" % ", " %!% args % ")"

		PAppCallApp superArity
		 -> "_callApp"	% length args % " (" % ", " %!% (ppr x : ppr superArity : map ppr args) % ")"

		PAppApply
		 -> "_apply"	% length args % " (" % x % ", " % ", " %!% args % ")"

		PAppCurry superArity
		 -> "_curry"	% length args % " (" % ", " %!% (ppr x : ppr superArity : map ppr args) % ")"

	-- Primitive allocation functions.
	XPrim (MAlloc prim) []
	 -> case prim of
		PAllocThunk f superArity argCount
	 	 -> "_allocThunk ((FunPtr) " % sV f % ", " % superArity % ", " % argCount % ")"
	
		PAllocData ctor ctorArity
		 -> "_allocData (" % "_tag" % sV ctor % ", " % ctorArity % ")"

	-- Primitive projections.
	XPrim (MProj f) [xCtor, xField, x]
	 -> case f of
	 	PProjField	-> "_FIELD("  % x % ", " % "_S" % xCtor % ", " % xField % ")"
	 	PProjFieldRef	-> "_FIELDR(" % x % ", " % "_S" % xCtor % ", " % xField % ")"

	-- Primitive functions in the RTS.
	XPrim (MFun f) [x1]
	 -> case f of
		PFunForce	-> "_force (" % x1 %")"
		_		-> panic stage ("ppr[Exp]: no match for " % show xx)

	XPrim (MFun f) [x1, x2]
	 -> case f of
		PFunArrayPeek t	-> "_arrayPeek (" % t % ", " % x1 % ", " % x2 % ")"
		PFunArrayPoke t	-> "_arrayPoke (" % t % ", " % x1 % ", " % x2 % ")"
		PFunStrCmp	-> "strcmp (" % x1 % ", " % x2 % ")"
		_		-> panic stage ("ppr[Exp]: no match for " % show xx)

	-- Primitive boxing functions.
	XPrim (MBox t) [x]
	 | t == TCon (TyConUnboxed  $ Var.primTBool   Unboxed)  
	 -> "_boxEnum(" % x % ")"

	 | t == TCon (TyConAbstract $ Var.primTString Unboxed) 
	 -> "Data_String_boxString(" % x % ")"
	
	 | otherwise
	 -> "_box(" % t % ", " % x % ")"

	XPrim (MUnbox t) [x]
	 | t == TCon (TyConUnboxed $ Var.primTBool  Unboxed)
	 -> "_unboxEnum(" % x % ")"

	 | otherwise
	 -> "_unboxDirect(" % t % ", " % x % ")"

	_ -> panic stage ("ppr[Exp]: no match for " % show xx)

-- Lit ---------------------------------------------------------------------------------------------
instance Pretty Lit PMode where
 ppr xx 
  = case xx of
	LNull		-> ppr "_null"
	LUnit 		-> ppr "_primUnit"
	LDataTag v	-> "_tag" % sV v
	LLit lit	-> pprLiteralFmt lit


-- Type --------------------------------------------------------------------------------------------
instance Pretty Type PMode where
 ppr xx
  = case xx of
	TVoid		-> ppr "void"

	TFun [] tRes	 
	 -> "() -> " % tRes

	TFun [tArg] tRes
	 -> pprTypeArg tArg % " -> " % tRes
	
	TFun tsArgs tRes
	 -> parens (punc ", " (map pprTypeArg tsArgs)) % " -> " % tRes

	TPtr x		-> x % "*"

	TCon tc		-> ppr tc

pprTypeArg :: Type -> Str
pprTypeArg tt
 = case tt of
	TFun{}	-> parens $ ppr tt
	_	-> ppr tt


-- TyCon ------------------------------------------------------------------------------------------
instance Pretty TyCon PMode where
 ppr xx
  = case xx of
	TyConObj	-> ppr "Obj"
	TyConAbstract v	-> ppr $ seaNameOfCtor v
	TyConUnboxed  v -> ppr $ seaNameOfCtor v
	


-- | Get the Sea name of a type constructor name.
seaNameOfCtor :: Var -> String
seaNameOfCtor var
	| [name]	<- [n | ISeaName n <- varInfo var]
	= name

 	| [var_binding]	<- [v | IBoundBy v <- varInfo var]
	, [name]	<- nub [n | ISeaName n <- varInfo var_binding]
	= name

 	| otherwise
	= panic stage
	$  "getSeaName: no sea name for TCon " % var % "\n"
	%  "  info = " % show (varInfo var) % "\n"


-- Literals ---------------------------------------------------------------------------------------
-- | Print a literal as a Sea expression.
pprLiteralFmt :: LiteralFmt -> Str
pprLiteralFmt litfmt@(LiteralFmt lit fmt)
 = case (lit, fmt) of

 	(LBool b, Unboxed)
	 -> case b of
	 	True	-> ppr "true"
		False	-> ppr "false"

	(LWord i,   UnboxedBits _)	-> ppr i
	(LInt i,    UnboxedBits _)	-> ppr i
	(LFloat f,  UnboxedBits _)	-> ppr f

	(LInt i,    Unboxed)		-> ppr i

	(LChar c,   UnboxedBits _)	-> ppr $ show c
	(LString s, Unboxed)		-> ppr $ show s
	_ -> panic stage $ "pprLiteralFmt: no match for " % show litfmt


-- Names ------------------------------------------------------------------------------------------
instance Pretty Name PMode where
 ppr nn 
  = case nn of
	NRts   v	-> ppr $ varName v
	NSuper v	-> ppr $ seaVar False v
	NAuto  v	-> ppr $ seaVar True  v
	NSlot  _ i	-> "_S(" % i % ")"
	NCafPtr v	-> ppr $  "*_ddcCAF_" ++ seaVar False v
	NCaf    v	
	 | isJust (takeSeaNameOfVar v)	-> ppr $ seaVar False v
	 | otherwise			-> ppr $ "_ddcCAF_" ++ seaVar False v
	

-- | Show the Sea name of a varaible.
--   The first argument says whether the variable is local to the current supercombinator,
--   and should not have a moduleid attached.
--   TODO: Change this so the way to print a variable can be termined directly from the Sea.Exp.
--
seaVar 	:: Bool -> Var -> String
seaVar local v

	-- If the variable has an explicit sea name embedded in it, then use that
	| name : _	<- [name | ISeaName name <- varInfo v]
	= name

	-- Binding occurance has an explicit Sea name, so use that.
	--	Used for calling foreign functions.
	| name : _	<- [name |  ISeaName name 
				 <- concat $ [varInfo bound | IBoundBy bound <- varInfo v]]
	= name

	| Var.varHasSymbols v
	= seaModule (varModuleId v)
	++ (if local 	then "_" ++ (pprStrPlain $ varId v) ++ "_" 
			else "_") 
	++ "_sym" ++ (Var.deSymString $ varName v)

	-- If the variable is explicitly set as global use the given name.
	| True : _	<- [global | ISeaGlobal global <- varInfo v]
	= varName v

	-- local vars are specific to a single Sea function.
	-- 	we need to prepend "_v" to avoid conflicts with C keywords
	--	and builtin functions from the RTS.
	| local
	= "_v" ++ varName v

	-- vars defined at top level need their module name prepended.
	| otherwise
	= seaModule (varModuleId v) ++ "_" ++ varName v


-- | Show a module id. 
seaModule :: ModuleId -> String
seaModule m
 = case m of
	ModuleIdNil	-> ""
	ModuleId ns	-> (catInt "_" $ ns)


