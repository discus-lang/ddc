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
(
	showSource
)

where

import Shared.Error
import Source.Token

-- mutual dependency.. sigh.
-- import qualified Shared.Error as Error


stage	= "Source.TokenShow"

-----
-- showSource
--	Shows a token in the same format as read from the source file
--	used for error reporting.
--
showSource tok =
 case tok of
 	Tycon	str	-> str
	Var	str	-> str
	Symbol	str	-> str
	CInt	i	-> show i
	CFloat  f	-> show f
	CString s	-> show s
	Junk    s	-> "<junk " ++ s ++ ">"
	
	tok		-> 
	 case lookup tok tokString of
	 	Just str	-> str
		Nothing		-> panic stage $ "showSource: unknown token: " ++ show tok

-----
tokString =
	[ (Foreign,		"foreign")
	, (Import, 		"import")
	, (Export,		"export")

	, (Module,		"module")
	, (Elaborate,		"elaborate")
	, (Const,		"const")
	, (Mutable,		"mutable")
	, (Extern,		"extern")
	, (CCall,		"ccall")

	, (Data, 		"data")
	, (Effect,		"effect")

	, (Class,		"class")
	, (Instance,		"instance")
	, (Project,		"project")
	
	, (InfixR, 		"infixr")
	, (InfixL,		"infixl")
	, (Infix,		"infix")

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

	, (Forall,		"forall")
	, (Dot,			".")

	, (HasType,		"::")
	, (HasOpType,		":$")
	, (HasConstraint,	":-")

	, (RightArrow,		"->")
	, (LeftArrow,		"<-")
	, (LeftArrowLazy,	"<@-")
	, (Unit,		"()")

	, (GuardCase,		"|-")
	, (GuardCaseC,		",-")
	
	, (GuardUnboxed,	"|#")
	, (GuardUnboxedC,	",#")
	
	, (GuardDefault,	"\\=")

	, (Hash,		"#")
	, (Star, 		"*")
	, (Dash,		"-")
	, (At,			"@")
	, (Bang,		"!")
	, (FSlash,		"/")
	, (Plus,		"+")
	, (Percent,		"%")

	, (AKet,		">")
	, (CBra,		"{")
	, (CKet,		"}")
	, (RBra,		"(")
	, (RKet,		")")
	, (SBra,		"[")
	, (SKet,		"]")
	, (BSlash,		"\\")
	, (BTick,		"'")
	, (Equals,		"=")
	, (Comma,		",")
	, (Colon,		":")
	, (SColon,		";")
	, (Bar,			"|") 
	, (Dollar,		"$")]




