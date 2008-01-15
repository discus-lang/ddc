{-# OPTIONS -fwarn-incomplete-patterns #-}

module Sea.Pretty
(
	seaVar
)

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

import Sea.Exp

stage	= "Sea.Pretty"

-----
maxSpecialApply	= 4
maxSpecialCurry	= 4

-----
sV  v		= ppr $ seaVar False v
sVn n v		= ppr $ padR n $ seaVar False v

sVL  v		= ppr $ seaVar True v
sVLn n v 	= ppr $ padR n $ seaVar True v

-----
instance Pretty a => Pretty (Top (Maybe a)) where
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
	PHackery s		-> ppr s
	PComment s		-> "// " % s
	PBlank			-> ppr "\n"
	PHashDef s1 s2		-> "#define " %  padR 8 s1 %>> " " % s2 % "\n"



-----
instance Pretty a => Pretty (Stmt (Maybe a)) where
 ppr xx
  = case xx of
	-- misc
	SBlank 			-> ppr " "
	SComment s		-> "// " % s

	-- append a "/* hackery */" comment here so that we know 
	--	this line is hackery and not a real statement. 
	SHackery str		-> "/**/ " % str

	-- stacks
	SAuto	v t		-> (padR 12 $ pprStr t) % " " % sVL v % ";"
	SEnter countS		-> "_ENTER (" % countS % ");"
	SLeave countS		-> "_LEAVE (" % countS % ");"

	-- assignment
	SAssign (XVar v) t x2	-> (sVLn 23 v) 			% " = " % x2 % ";"
	SAssign x1 t x2		-> (padR 23 $ pprStr x1) 	% " = " % x2 % ";"

	SStmt s			-> ppr s % ";"

	-- control flow
	SReturn x		-> "return " % x % ";"
	SLabel v		-> sV v % ":"
	SGoto v			-> "goto " % sV v % ";"

	SSwitch x aa
	 -> "switch (" % x % ") {\n"
	    % (appendMapPretty aa)
	    % "}"

	SMatch aa
	 -> "match {\n"
	    % (appendMapPretty aa)
	    % "}"
	

-----
instance Pretty a => Pretty (Alt (Maybe a))where
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

-----
instance Pretty a => Pretty (Guard (Maybe a)) where
 ppr gg
  = case gg of
  	GCase ss x1 x2
	 -> "guard {\n"
	 %> ("\n" %!% ss % "\n") % "}\n"
	 %  "compare " % x1 % " with " % x2 % ";\n";
	 


-----
instance Pretty a => Pretty (Exp (Maybe a))where
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
	
	-- projection
	XTag x
	 -> "_TAG(" % x % ")"

	XArg x t i
	 -> case t of
	 	TData		-> "_DARG(" % x % ", " % i % ")"
		TThunk		-> "_TARG(" % x % ", " % i % ")"
		TSusp 	 	-> "_SARG(" % x % ", " % i % ")"

	XField x v l		-> "_FIELD("  % x % ", " % "_S" % sV v % ", " % l % ")"
	XFieldR x v l		-> "_FIELDR(" % x % ", " % "_S" % sV v % ", " % l % ")"

	-- constants	 
	XCon v			-> "_tag" % sV v
	XInt i			-> ppr i
	XUnit 			-> ppr "_primUnit"
	XLiteral lit		-> makeLiteral lit
	XTagThunk		-> ppr "_tagThunk"
	XSuper v		-> "(void*)" % sV v
	XNull			-> ppr "_null"
	XAtom v			-> "_atom" % sV v

	-- boxing
	XBox t x
	 | t == TCon Var.primTBoolU []
	 -> "_boxEnum(" % x % ")"
	 
	 | otherwise
	 -> "_box(" % t % ", " % x % ")"
	  
	XUnbox t x
	 |  t == TCon Var.primTBoolU []
	 -> "_unboxEnum(" % x % ")"
	 
	 | otherwise
	 -> "_unbox(" % t % ", " % x % ")"
	  
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
	 -> "_allocDataAnchored (" % "_tag" % sV ctor % ", " % arity % ")"

	XAllocSusp  thunk arity
	 -> "_allocSusp (" % sV thunk % ", " % arity % ")"

	_ -> panic stage $ "pprStr[Exp]: no match for " % show xx

-----
instance Pretty Type where
 ppr xx
  = case xx of
	TVoid		-> ppr "void"
	TObj		-> ppr "Obj*"

	TCon v [t]
	 | v == Var.primTPtrU
	 ->  t % "*"
	 
	TCon v ts
	 -> let ps	= splitOns '.' $ Var.name v
	    in	ppr $ init (last ps) % " " % " " %!% ts
	 		
	_ -> panic stage $ "pprStr[Type]: no match for " % show xx

makeLiteral lit
 = case lit of
	LChar	c	-> ppr $ show c
	LString s	-> ppr $ show s
 	LInt	i	-> ppr i 
	LFloat	f	-> ppr f


-----
seaVar :: 	Bool -> Var -> String
seaVar local v
	| name : _	<- [name | Var.ISeaName name <- Var.info v]
	= name
		
	| name : _	<- [name |  Var.ISeaName name
				 <- concat $ [Var.info bound | Var.IBoundBy bound <- Var.info v]]
	= name
	
	|    Var.isSymbol v
	  || (elem '\'' $ Var.name v)
	= seaModule (Var.nameModule v)
	++ (if local then "_" ++ (pprStr $ Var.bind v) ++ "_" else "")
	++ "_sym" ++ (Var.deSymString $ Var.name v)	

	| Var.isDummy v
	, Var.XBind{}	<- Var.bind v
	= seaModule (Var.nameModule v)
	++ "_" ++ Var.name v
			
	| otherwise
	= seaModule (Var.nameModule v)
--	++ (if local then "_" ++ (pprStr $ Var.bind v) ++ "_" else "")
	++ Var.name v


seaModule ::	Module -> String
seaModule	m
 = case m of
	ModuleNil		-> ""
	ModuleAbsolute ns	-> (catInt "_" $ ns) ++ "_"


--	= show (Var.nameModule v) ++ Var.name v
--	= Var.name v ++ (show $ Var.info v)

