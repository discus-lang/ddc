-----
-- Source.TokenShow
--
-- Summary:
--	Pretty printer for tokens.
--	Prints tokens in same format as they were read from the source file.
--	Useful for generating parser error messages.
--
--
module Source.TokenShow
	( showSource 
	, showSourceP)
where
import Source.Token
import Shared.Pretty
import Shared.Error
import Util

-----
stage	= "Source.TokenShow"


-- | Show a token in the same format as read from the source file
--   used for error reporting.
showSource tok =
 case tok of
	ModuleName strs	-> concat $ intersperse "." strs
	Var	str	-> str
	VarField str	-> "_" ++ str 
	Con	str	-> str
	Symbol	str	-> str

	Literal lf	-> pprStrPlain $ ppr lf

	Junk    s	-> "<junk " ++ s ++ ">"
	CommentPragma s	-> s
	tok		-> 
	 case lookup tok tokString of
	 	Just str	-> str
		Nothing		-> panic stage $ "showSource: unknown token: " ++ show tok

showSourceP tok = 
 case tok of
 	TokenP{}	-> showSource (token tok)
	_		-> show tok


-- | Source versions of each token
--	Keep these in the same order as in Token.hs.
tokString =
	-- Weak keywords ---------------------------------------
	[ (Module,		"module")
	, (Elaborate,		"elaborate")
	, (Const,		"const")
	, (Mutable,		"mutable")
	, (Extern,		"extern")
	, (CCall,		"ccall")

	-- Regular keywords ------------------------------------
	, (Pragma,		"pragma")
	
	-- infix definitions -----------------------------------
	, (InfixR, 		"infixr")
	, (InfixL,		"infixl")
	, (Infix,		"infix")

	-- module definitions ----------------------------------
	, (Foreign,		"foreign")
	, (Import, 		"import")
	, (Export,		"export")

	-- type definitions ------------------------------------
	, (Type,		"type")
	, (Data, 		"data")
	, (Region,		"region")
	, (Effect,		"effect")
	, (Class,		"class")
	, (Instance,		"instance")
	, (Project,		"project")
	, (Forall,		"forall")

	-- expressions -----------------------------------------
	, (Let,			"let")
	, (In,			"in")
	, (Where,		"where")

	, (Case,		"case")
	, (Of,			"of")
	, (Match,		"match")

	, (If,			"if")
	, (Then,		"then")
	, (Else,		"else")

	, (Try,			"try")
	, (Throw,		"throw")
	, (Catch,		"catch")
	, (With,		"with")

	, (Do,			"do")
	, (While,		"while")
	, (When,		"when")
	, (Unless,		"unless")
	, (Break,		"break")

	-- Symbols -----------------------------------------
	
	-- type symbols 
	, (HasType,		"::")
	, (IsSubtypeOf,		"<:")
	, (IsSuptypeOf,		":>")

	, (HasOpType,		":$")
	, (HasConstraint,	":-")

	, (RightArrow,		"->")
	, (RightArrowEquals,	"=>")
	, (HoldsMono,		"$>")

	-- shared between types and expressions 
	, (LeftArrow,		"<-")
	, (Unit,		"()")
	

	-- expression symbols 
	, (LeftArrowLazy,	"<@-")

	, (GuardCase,		"|-")
	, (GuardCaseC,		",-")
	, (GuardUnboxed,	"|#")
	, (GuardUnboxedC,	",#")
	, (GuardDefault,	"\\=")

	, (BackSlashDot,	"\\.")

	, (Dot,			".")
	, (DotDot,		"..")

	, (Hash,		"#")
	, (Star, 		"*")
	, (Dash,		"-")
	, (Plus,		"+")
	, (Percent,		"%")
	, (At,			"@")
	, (Bang,		"!")
	, (Dollar,		"$")
	, (Tilde,		"~")
	, (Underscore,		"_")
	, (Hat,			"^")
	, (ForwardSlash,	"/")
	, (BackSlash,		"\\")
	, (BackTick,		"'")
	, (Equals,		"=")
	, (Comma,		",")
	, (Colon,		":")
	, (SemiColon,		";")
	, (Bar,			"|") 
	, (And,			"&")

	-- parenthesis
	, (AKet,		">")
	, (ABra,		"<")

	, (CBra,		"{")
	, (CKet,		"}")

	, (RBra,		"(")
	, (RKet,		")")

	, (SBra,		"[")
	, (SKet,		"]") 
	
	, (NewLine,		"\n")
	, (CommentLineStart,	"@CommentLineStart")
	, (CommentBlockStart,	"@CommentBlockStart")
	, (CommentBlockEnd,	"@CommentBlockEnd") ]
	
	



