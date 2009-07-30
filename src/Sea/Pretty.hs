{-# OPTIONS -fwarn-incomplete-patterns -O2 #-}

module Sea.Pretty
	( seaVar )
where
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import qualified Shared.VarPrim	as Var
import Shared.Var		(Var, NameSpace(..), Module(..))
import Shared.Pretty
import Shared.Error
import Shared.Literal
import Shared.Base

import Sea.Exp

stage	= "Sea.Pretty"

-----
sV  v		= ppr $ seaVar False v
sVn n v		= ppr $ padL n $ seaVar False v

sVL  v		= ppr $ seaVar True v
sVLn n v 	= ppr $ padL  n $ seaVar True v


-- Top ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Top (Maybe a)) PMode where
 ppr xx
  = case xx of
	PNil	 		-> ppr $ "$PNil\n"
	PData v vds		-> "@Data" % sV v % vds

	-- supers
	PProto v argTypes resultType
	 -> resultType %>> sVn 8 v %>> "(" % ", " %!% argTypes % ");\n"

	PSuper v vts rt ss
	 -> rt %>> sV v %>>"(" % ", " %!% (map (\(av, at) -> at % " " % sVL av) vts) % ")" % "\n"
	  % "{\n"
	  	%> ("\n" %!% ss % "\n")
	  % "}\n\n\n"

	-- cafs
	PCafProto v
	 -> "extern Obj** " 	%>> "_ddcCAF_" % sV v % ";\n\n"

	PCafSlot  v 
	 -> "Obj** " 		%>> "_ddcCAF_" % sV v % " = 0;\n"

	PCafInit v ss	
	 -> "void " %>> "_ddcInitCAF_" % sV v %>> "()\n"
	 % "{\n"
	 	%> ("\n" %!% ss % "\n")
	 % "}\n\n\n"
	

	-- constructors
	PCtor v  argVs   resultV
	 -> "@PCtor " % v %>> ":$ " % " -> " %!% (argVs ++ [resultV]) % ";\n"


	-- atoms
	PAtomProto v t	-> "extern " % t % " _atom" % sV v % ";\n"
	PAtom v t	-> t % " _atom" % sV v % " = 0;\n" 

	-- structs
	PStruct v cs
	 -> "struct " % "_S" % sV v % "\n"
		% "{\n"
		%>> "Tag " 	%>> "_tag;\n"
		%>> "UInt "	%>> "_size;\n"
		%> ("\n" %!% map (\(v, t) -> t % " " %>> sVL v % ";") cs) % "\n"
		% "};\n\n"

	-- Sea hackery.
	PInclude s		-> "#include <" % s % ">\n"
	PIncludeAbs s		-> "#include \"" % s % "\"\n"
	PHackery s		-> ppr s
	PComment s		-> "// " % s
	PBlank			-> ppr "\n"
	PHashDef s1 s2		-> "#define " %  padL 8 s1 %>> " " % s2 % "\n"



-- Stmt --------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Stmt (Maybe a)) PMode where
 ppr xx
  = case xx of
	-- misc
	SBlank 			-> ppr " "
	SComment s		-> "// " % s

	-- append a "/* hackery */" comment here so that we know 
	--	this line is hackery and not a real statement. 
	SHackery str		-> "/**/ " % str

	-- stacks
	SAuto	v t		-> (padL 12 $ pprStrPlain t) % " " % sVL v % ";"
	SEnter countS		-> "_ENTER (" % countS % ");"
	SLeave countS		-> "_LEAVE (" % countS % ");"

	-- assignment
	SAssign (XVar v) t x2	-> (sVLn 23 v) 			% " = " % x2 % ";"
	SAssign x1 t x2		-> (padL 23 $ pprStrPlain x1) 	% " = " % x2 % ";"

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

	ACaseSusp x l		-> "  _CASESUSP (" % x % ", " % "_" % l % ");\n"
	ACaseDeath		-> ppr "  _CASEDEATH;\n"


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
  	GCase True ss x1 x2
	 -> "guard {\n"
	 %> ("\n" %!% ss % "\n") % "}\n"
	 %  "compareLazy " % x1 % " with " % x2 % ";\n";

  	GCase False ss x1 x2
	 -> "guard {\n"
	 %> ("\n" %!% ss % "\n") % "}\n"
	 %  "compareDirect " % x1 % " with " % x2 % ";\n";
	 

-- Exp ---------------------------------------------------------------------------------------------
instance Pretty a PMode => Pretty (Exp (Maybe a)) PMode where
 ppr xx
  = case xx of
  	XNil			-> ppr "$XNil"
	XVar v			-> sVL v
	XSlot    v i		-> "_S(" % i % ")"
	XSlotCAF v 		-> "_CAF(" % sV v % ")"

	-- application
	XTailCall v args
	 -> "@XTailCall " % sV v % " (" % ", " %!% args % ")"

	XCall v args
	 -> sV v % " (" % ", " %!% args % ")"

	XCallApp v superAirity args
	 -> let i 	= length args
	    in  "_callApp"	% i % " (" % ", " %!% (sV v : ppr superAirity : map ppr args) % ")"

	XApply x args
	 -> let i	= length args
	    in  "_apply"	% i % " (" % x % ", " % ", " %!% args % ")"
	    
	
	XCurry v superAirity args
	 -> let i	= length args
	    in "_curry"		% i % " (" % ", " %!% (sV v : ppr superAirity : map ppr args) % ")"
	
	XSuspend v args
	 -> let i	= length args
	    in "_suspend"	% i % " (" % ", " %!% (sV v : map ppr args) % ")"

	XPrim f [(XVar ctorV), (XVar fieldV), x]
	 -> case f of
	 	FProjField	->  "_FIELD(" % x % ", " % "_S" % sV ctorV % ", " % fieldV % ")"
	 	FProjFieldR	-> "_FIELDR(" % x % ", " % "_S" % sV ctorV % ", " % fieldV % ")"
		_		-> panic stage ("ppr[Exp]: no match for " % show xx)

	XPrim f [x1]
	 |  f == FNeg
	 -> "-(" % x1 % ")"

	XPrim f [x1, x2]
	 -> case f of
	 	FAdd		-> "(" % x1 % " + "	% x2 % ")"
		FSub		-> "(" % x1 % " - "	% x2 % ")"
		FMul		-> "(" % x1 % " * "	% x2 % ")"
		FDiv		-> "(" % x1 % " / "	% x2 % ")"
		FMod		-> "(" % x1 % " % "	% x2 % ")"
	 	FEq		-> "(" % x1 % " == "	% x2 % ")"
		FNEq		-> "(" % x1 % " != "	% x2 % ")"

	 	FGt		-> "(" % x1 % " > "	% x2 % ")"
	 	FLt		-> "(" % x1 % " < "	% x2 % ")"
	 	FGe		-> "(" % x1 % " >= "	% x2 % ")"
	 	FLe		-> "(" % x1 % " <= "	% x2 % ")"

		FAnd		-> "(" % x1 % " && "	% x2 % ")"
		FOr		-> "(" % x1 % " || "	% x2 % ")"

		FArrayPeek t	-> "_arrayPeek (" % t % ", " % x1 % ", " % x2 % ")"
		FArrayPoke t	-> "_arrayPoke (" % t % ", " % x1 % ", " % x2 % ")"

		FStrCmp		-> "strcmp (" % x1 % ", " % x2 % ")"

		_		-> panic stage ("ppr[Exp]: no match for " % show xx)
	
	-- projection
	XTag x
	 -> "_TAG(" % x % ")"

	XArg x t i
	 -> case t of
	 	TData		-> "_DARG(" % x % ", " % i % ")"
		TThunk		-> "_TARG(" % x % ", " % i % ")"
		TSusp 	 	-> "_SARG(" % x % ", " % i % ")"
		_		-> panic stage ("ppr[Exp]: no match for " % show xx)

	XField x v l		-> "_FIELD("  % x % ", " % "_S" % sV v % ", " % l % ")"
	XFieldR x v l		-> "_FIELDR(" % x % ", " % "_S" % sV v % ", " % l % ")"

	-- constants	 
	XCon v			-> "_tag" % sV v
	XInt i			-> ppr i
	XUnit 			-> ppr "_primUnit"
	XLit lit		-> pprLiteralFmt lit
	XTagThunk		-> ppr "_tagThunk"
	XSuper v		-> "(void*)" % sV v
	XNull			-> ppr "_null"
	XAtom v			-> "_atom" % sV v

	-- boxing
	XBox t x
	 | t == TCon (Var.primTBool Unboxed) []
	 -> "_boxEnum(" % x % ")"
	 
	 | t == TCon (Var.primTString Unboxed) []
	 -> "Data_String_boxString(" % x % ")"
	 
	 | otherwise
	 -> "_box(" % t % ", " % x % ")"
	  
	XUnbox t x
	 |  t == TCon (Var.primTBool Unboxed) []
	 -> "_unboxEnum(" % x % ")"
	 
	 | otherwise
	 -> "_unboxDirect(" % t % ", " % x % ")"
	  
	XForce x 
	 -> "_force(" % x % ")"
	
	-- allocation
	XAlloc i		
	 -> "_alloc (" % i % ")"
	
	XAllocThunk f superA argCount
	 -> "_allocThunk (" % sV f % ", " % superA % ", " % argCount % ")"

	XAllocData  ctor arity	
	 -> "_allocData (" % "_tag" % sV ctor % ", " % arity % ")"

	XAllocDataAnchored ctor arity
	 -> "_allocData_anchored (" % "_tag" % sV ctor % ", " % arity % ")"

	XAllocSusp  thunk arity
	 -> "_allocSusp (" % sV thunk % ", " % arity % ")"

	_ -> panic stage $ "pprStr[Exp]: no match for " % show xx


-- Type --------------------------------------------------------------------------------------------
instance Pretty Type PMode where
 ppr xx
  = case xx of
	TVoid		-> ppr "void"
	TObj		-> ppr "Obj*"

	TPtr x		-> x % "*"

	TCon var []
	 -> ppr $ getSeaName var
		 		
	_ -> panic stage $ "pprStr[Type]: no match for " % show xx

getSeaName :: Var -> String
getSeaName var
	| [name]	<- [n | Var.ISeaName n <- Var.info var]
	= name

 	| [var_binding]	<- [v | Var.IBoundBy v <- Var.info var]
	, [name]	<- nub [n | Var.ISeaName n <- Var.info var_binding]
	= name

 	| otherwise
	= panic stage 
		$  "getSeaName: no sea name for TCon " % var % "\n"
		%  "  info = " % show (Var.info var) % "\n"
	

pprLiteralFmt litfmt@(LiteralFmt lit fmt)
 = case (lit, fmt) of

 	(LBool b, Unboxed)
	 -> case b of
	 	True	-> ppr "true"
		False	-> ppr "false"

	(LWord i,   UnboxedBits b)	-> ppr i
	(LInt i,    UnboxedBits b)	-> ppr i
	(LFloat f,  UnboxedBits b)	-> ppr f

	(LChar c,   UnboxedBits b)	-> ppr $ show c
	(LString s, Unboxed)		-> ppr $ show s
	_ -> panic stage $ "pprLiteralFmt: no match for " % show litfmt


-----
seaVar :: Bool -> Var -> String
seaVar local v

	-- If the variable has an explicit sea name embedded in it, then use that
	| name : _	<- [name | Var.ISeaName name <- Var.info v]
	= name
	
	-- Binding occurance has an explicit Sea name, so use that.
	--	Used for calling foreign functions.
	| name : _	<- [name |  Var.ISeaName name
				 <- concat $ [Var.info bound | Var.IBoundBy bound <- Var.info v]]
	= name
	
	| Var.varHasSymbols v
	= seaModule (Var.nameModule v)
	++ (if local then "_" ++ (pprStrPlain $ Var.bind v) ++ "_" else "_")
	++ "_sym" ++ (Var.deSymString $ Var.name v)	

	-- local vars are specific to a single Sea function.
	-- 	we need to prepend "_v" to avoid conflicts with C keywords
	--	and builtin functions from the RTS.
	| local
	= "_v" ++ Var.name v
	
	-- vars defined at top level need their module name prepended.
	| otherwise
	= seaModule (Var.nameModule v) ++ "_" ++ Var.name v
	

seaModule :: Module -> String
seaModule m
 = case m of
	ModuleNil		-> ""
	ModuleAbsolute ns	-> (catInt "_" $ ns)
	_			-> panic stage $ "seaModule: no match for: " % show m


