{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -O2 #-}

-- | Pretty printing of Sea expressions.
module DDC.Sea.Pretty
	( seaVar
	, ctorName )
where
import DDC.Sea.Exp
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Base.DataFormat
import DDC.Base.SourcePos
import DDC.Base.Literal
import DDC.Base.Prim
import DDC.Var
import qualified Shared.VarUtil	as Var
import qualified Shared.VarPrim	as Var
import qualified Data.Map	as Map
import Data.Function
import Util

stage	= "DDC.Sea.Pretty"

-- Show global sea vars
sV  v		= ppr $ seaVar False v
sVn n v		= ppr $ padL n $ seaVar False v

-- Show local sea vars
sVL  v		= ppr $ seaVar True v


-- Top ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Top (Maybe a)) PMode where
 ppr xx
  = case xx of
	PNil	 	-> panic stage "ppr[Sea.Exp]: got pNil"

	-- Comments
	PComment []	-> ppr "//"
	PComment s	-> "// " % s % ""
	PBlank		-> nl

	-- External functions and data.
	PExtern v t -> "/* " %% sV v %% ":: " %% t %% "*/"

	-- Data type declarations.
	PData v ctors
	 | Map.null ctors
	 -> "data" %% ppr v % ";"

	 | otherwise
	 -> let ctorsList = sortBy (compare `on` ctorDefTag) $ Map.elems ctors
	    in  pprHeadBraces ("data" %% v %% "where" % nl)
			ctorsList

	PCtorStruct n f
	 -> let fields = map (\(i, t) -> ppr t %% "f" % show i % ";") f
	    in "typedef struct {" % nl % indent (vcat fields) % "\n}" %% ctorName n %% ";\n"

	-- Constructor tag name and value
	PCtorTag n i	-> "#define _tag" %  n % " (_tagBase + " % i % ")"

	-- Supercombinators
	PProto v argTypes resultType
	 -> let params = if null argTypes
				then ppr "void"
				else punc ", " argTypes
	    in resultType %>> sVn 40 v %>> parens (params) % ";"

	PSuper vName vtsArgs tResult stmts
	 -> let params = if null vtsArgs
				then ppr "void"
				else punc ", " [at %% sVL av | (av, at) <- vtsArgs]
	    in pprHeadBraces
		(tResult %>> sV vName %>> parens (params) %  nl)
		stmts
		% nl

	-- CAFs
	PCafProto v t
	 | not $ typeIsBoxed t	-> "extern" %% t %>> " _ddcCAF_" % sV v % ";"
	 | otherwise		-> "extern" %% t %>> " *_ddcCAF_" % sV v % ";"

	PCafSlot  v t
	 | not $ typeIsBoxed t	-> t %>> " _ddcCAF_"  % sV v % " = 0;"
	 | otherwise		-> t %>> " *_ddcCAF_" % sV v % " = 0;"

	PCafInit v _ stmts
	 -> pprHeadBraces
		("void " %>> "_ddcInitCAF_" % sV v %>> "()")
		stmts
	 % nl

	-- main entry point.
	PMain mainModuleName modulesImported withHandler
		mHeapSize mSlotStackSize mContextStackSize

	 -> let	heapSize	 = fromMaybe 0 mHeapSize
		slotStackSize	 = fromMaybe 0 mSlotStackSize
		contextStackSize = fromMaybe 0 mContextStackSize

	    in	"int main (int argc, char** argv)" % nl
	 	% "{" % nl
		% (indent $ vcat
			[ ppr "// Initialise the runtime system"
			, "_ddcRuntimeInit(argc, argv,"
				%% heapSize 		% ","
				%% slotStackSize	% ","
				%% contextStackSize	% ");"
			, blank

			, ppr "// Initialise all the imported modules"
			, vcat [ fn % "();" | fn <- modulesImported ]
			, blank

			, ppr "// Initialise the main module"
			, "_ddcInitModule_" % mainModuleName % "();"
			, blank

			, ppr "// Run the client program"
			, if withHandler
				then ppr $ "Control_Exception_topHandle(_allocThunk((FunPtr) "
						% mainModuleName % "_main, 1, 0));"
				else ppr $ "Main_main(Base_Unit());"
			 , blank

			, ppr "// Shut down the runtime system"
			, ppr "_ddcRuntimeCleanup();"
			, blank
			, ppr "return 0;" ])
		% nl % "}" % nl

-- CtorDef --------------------------------------------------------------------------------------------
instance Pretty CtorDef PMode where
 ppr xx
  = case xx of
  	CtorDef v t arity tag fs _ts
 	 -> v 	% nl
		%> 	( ":: " % ppr t % nl
			% "with { ARITY  = " % arity	% nl
 			% "     , TAG    = " % tag      % nl
			% "     , FIELDS = " % fs 		% "}")


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
	 -> pprHeadBraces ("switch" %% parens x % " ") aa

	SMatch aa
	 -> pprHeadBraces "match" aa

	SIf xExp ssThen
	 -> pprHeadBraces ("if" %% parens xExp) ssThen

	SCaseFail (SourcePos (f, l, c))
	 -> ppr "_CASEFAIL (\"" % windowsPathFix f % "\", " % l % ", " % c % ");"


-- Alt ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Alt (Maybe a)) PMode where
 ppr xx
  = case xx of
	AAlt gs ss
	 -> "  alt:  "
	 %> (vcat gs % nl % "then" % brackets (vcat ss))
	 % nlnl

	ASwitch g []
	 -> "  case" %% g % ": break;"

	ASwitch g [SGoto v]
	 -> "  case" %% g % ": goto " % sV v % ";"

	ASwitch g ss
	 -> pprHeadBraces ("  case " % g % ":")
	 	(map ppr ss ++ [ppr "break"])

	ACaseSusp x l
	 -> "  _CASESUSP (" % x % ", " % "_" % l % ");"

	ACaseIndir x l
	 -> "  _CASEINDIR (" % x % ", " % "_" % l % ");"

	ACaseDeath (SourcePos (f, l, c))
	 -> ppr "  _CASEDEATH (\"" % windowsPathFix f % "\", " % l % ", " % c % ");"

	ADefault [SGoto v]
	 -> "  default: goto " % sV v % ";"

	ADefault ss
	 -> pprHeadBraces ("  default:")
		(map ppr ss ++ [ppr "block"])


-- Guard -------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Guard (Maybe a)) PMode where
 ppr gg
  = case gg of
  	GCase _ True ss x1 x2
	 -> pprHeadBraces "guard" ss
	 %  "compareLazy"    %% x1 % " with " % x2 % ";";

  	GCase _ False ss x1 x2
	 -> pprHeadBraces "guard" ss
	 %  "compareDirect"  %% x1 % " with " % x2 % ";";


-- Exp ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Exp (Maybe a)) PMode where
 ppr xx
  = case xx of
  	XNil		-> ppr "@XNil"

	-- variables
	XVar name typ
	 -> pprIfMode (elem PrettySeaTypes)
		(parens $ ppr name % " :: " % ppr typ)
		(ppr name)

	-- projection
	XTag x
	 -> "_getObjTag(" % x % ")"

	XArgData x@(XVar _ _) i
	 -> "_DARG(" % x % ", " % i % ")"

	XArgDataM v x@(XVar _ _) i
	 -> "_DMARG(" % x % ", " % ctorName v % ", " % i % ")"

	XArgThunk x@(XVar _ _) i
	 -> "_TARG(" % x % ", " % i % ")"

	-- literals
	XLit lit	-> ppr lit

	-- Primitives ---------------------------------------------------------
	-- unary arithmetic operators.
	XPrim (MOp f) [x1]
	 -> case f of
		OpNeg	 -> "-(" % x1 % ")"
		OpIsZero -> parens $ x1 % " == 0"
		_	 -> panic stage $ "ppr[Exp]: no match for " % show xx

	-- binary arithmetic operators.
	XPrim (MOp f) [x1, x2]
	 -> case f of
	 	OpAdd	-> parens $ x1 % " + "	% x2
		OpSub	-> parens $ x1 % " - "	% x2
		OpMul	-> parens $ x1 % " * "	% x2
		OpDiv	-> parens $ x1 % " / "	% x2
		OpMod	-> parens $ x1 % " % "	% x2
	 	OpEq	-> parens $ x1 % " == "	% x2
		OpNeq	-> parens $ x1 % " != "	% x2

	 	OpGt	-> parens $ x1 % " > "	% x2
	 	OpLt	-> parens $ x1 % " < "	% x2
	 	OpGe	-> parens $ x1 % " >= "	% x2
	 	OpLe	-> parens $ x1 % " <= "	% x2

		OpAnd	-> parens $ x1 % " && "	% x2
		OpOr	-> parens $ x1 % " || "	% x2
		_	-> panic stage $ "ppr[Exp]: no match for " % show xx

	-- Primitive function application operators.

	XPrim (MApp PAppCall) (x@(XVar _ (TFun [TVoid] _)) : [])
	 -> x % " ()"

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
		PAllocThunk f _ superArity argCount
	 	 -> "_allocThunk ((FunPtr) " % sV f % ", " % superArity % ", " % argCount % ")"

		PAllocData ctor ctorArity
		 -> "_allocData (" % "_tag" % sV ctor % ", " % ctorArity % ")"

		PAllocDataM ctor boxedObjs unboxedStructSize
		 -> "_allocDataM (" % "_tag" % sV ctor % ", " % boxedObjs %  ", " % unboxedStructSize % ")"

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

	-- Casting between numeric types.
	XPrim (MCast (PrimCast pt1 pt2)) [x]
	 -> "_CAST"
		% "("  % pprPrimType pt1
		% ", " % pprPrimType pt2
		% ", " % x
		% ")"

	-- Coercion between pointer types
	XPrim (MCoerce (PrimCoercePtr t1 t2)) [x]
	 -> "_COERCE_PTR" % (t1, t2, x)

	-- Coercion between pointer types
	XPrim (MCoerce (PrimCoerceAddrToPtr t1)) [x]
	 -> "_COERCE_ADDR_TO_PTR" % (t1, x)

	-- Coercion between pointer types
	XPrim (MCoerce (PrimCoercePtrToAddr t1)) [x]
	 -> "_COERCE_PTR_TO_ADDR" % (t1, x)

	-- Pointer addition
	XPrim (MPtr PrimPtrPlus) [x1, x2]
	 -> "_PLUSPTR" % (x1, x2)

	-- Pointer Poking.
	XPrim (MPtr (PrimPtrPokeOn _)) [x1, x2, x3]
	 -> "_POKEON" % (x1, x2, x3)

	XPrim (MPtr (PrimPtrPoke _))   [x1, x2]
	 -> "_POKE" % (x1, x2)

	-- Pointer peeking
	XPrim (MPtr (PrimPtrPeekOn _)) [x1, x2]
	 -> "_PEEKON" % (x1, x2)

	XPrim (MPtr (PrimPtrPeek _))   [x1]
	 -> "_PEEK" % parens x1


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


-- PrimType ---------------------------------------------------------------------------------------
-- | Pretty print a `PrimType` in Sea syntax.
pprPrimType :: PrimType -> Str
pprPrimType pt
 = case pt of
	PrimTypeAddr		-> ppr "Addr"
	PrimTypeWord  (Width w)	-> "Word"  % w
	PrimTypeInt   (Width w)	-> "Int"   % w
	PrimTypeFloat (Width w)	-> "Float" % w


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
	$  "getSeaName: no sea name for TCon " % var % nl
	%  "  info = " % show (varInfo var) % nl


-- Literals ---------------------------------------------------------------------------------------
-- | Print a literal as a Sea expression.
pprLiteralFmt :: LiteralFmt -> Str
pprLiteralFmt litfmt@(LiteralFmt lit fmt)
 = case (lit, fmt) of
 	(LBool b, Unboxed)
	 -> case b of
	 	True	-> ppr "true"
		False	-> ppr "false"

	(LWord i,   UnboxedBits 64)	-> "_DDC_UINT64_LITERAL (" % ppr i % ")"
	(LInt i,    UnboxedBits 64)	-> "_DDC_INT64_LITERAL (" % ppr i % ")"

	(LWord i,   UnboxedBits _)	-> ppr i
	(LInt i,    UnboxedBits _)	-> ppr i

	(LFloat f,  UnboxedBits _)	-> ppr f

	(LChar c,   UnboxedBits 32)	-> ppr $ show c
	(LString s, Unboxed)		-> ppr $ show s

	-- All unboxed literals from the Disciple source program are defaulted
	-- to have a specific size, like Int32. However, the Sea stages themselves
	-- can introduce unsized integers for various counters and offsets.
	(LInt i,    Unboxed)		-> ppr i

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


-- | Show a Var name as a constructor struct name.
ctorName :: Var -> String
ctorName v
 = "_" ++ seaModule (varModuleId v) ++ "_" ++ varName v ++ "_Ctor"


-- | Show a module id.
seaModule :: ModuleId -> String
seaModule m
 = case m of
	ModuleIdNil	-> ""
	ModuleId ns	-> (catInt "_" $ ns)


-- Hack fixup windows file paths.
windowsPathFix :: String -> String
windowsPathFix f = map (\z -> if z == '\\' then '/' else z) f
