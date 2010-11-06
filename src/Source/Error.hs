{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- Pretty printing of parse and renamer errors in the source program.
module Source.Error
	( Error(..))
where
import Util
import Source.Token
import DDC.Type
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Var
import Shared.VarUtil			(prettyPos, isCtorName)
import qualified Source.TokenShow 	as Token


-- | All the errors that the parser and renamer can generate
data Error
	-- Parser Errors ------------------------------------------------------
	-- | Nested block starts to left of enclosing one. 
	= ErrorLayoutLeft		TokenP

	-- | Explicit close brace must match explicit open brace. 
	| ErrorLayoutNoBraceMatch 	TokenP

	-- | Found a token which is a string with tabs in it.
	| ErrorLexicalStringTabs	TokenP

	-- | Tried to expand a string with a bad escape character.
	| ErrorLexicalEscape		TokenP

	-- | Parse error on this token.
	| ErrorParse			TokenP String

	-- | Parse error at this position.
	| ErrorParsePos			SourcePos String

	-- | Parser error sometime before these tokens.
	| ErrorParseBefore		[TokenP]

	-- | Parse error at end of input.
	| ErrorParseEnd		

	
	-- Defixer Errors -----------------------------------------------------
	-- | Can't have > 1 non-assoc op in the same string.
	| ErrorDefixNonAssoc		[Var]

	-- | Can't have ops of mixed assoc but same precedence in same string.
	| ErrorDefixMixedAssoc		[Var]

	
	-- Renamer Errors -----------------------------------------------------
	-- | Variable is undefined / out of scope.
	| ErrorUndefinedVar	
		{ eUndefined		:: Var }

	-- | Occurance of a type variable in a TForall shadows another
	| ErrorShadowForall
		{ eShadowVar		:: Var }

	-- | Variable is redefined in the same scope.
	| ErrorRedefinedVar	
		{ eFirstDefined		:: Var
		, eRedefined		:: Var }


	-- | An ambiguous binding occurrence.
	| ErrorAmbiguousVar
		{ eBindingVars		:: [Var]
		, eBoundVar		:: Var }

	-- Lint Errors --------------------------------------------------------
	| ErrorSigLacksBinding
		{ eSigVar		:: Var }

	-- Desugaring Errors --------------------------------------------------
	-- TODO: These errors emitted by the desugar stages and should
	--	really be in a different module, or picked up in Source.Lint
	-- | A projection name clashes with a field name in the same Data namespace.
	| ErrorProjectRedefDataField
		{ eFirstDefined		:: Var
		, eRedefined		:: Var
		, eDataVar		:: Var }

	-- | A projection name clashes with a field name in the same Data namespace.
	| ErrorRedefClassInst
		{ eClass		:: Var
		, eFirstDefined		:: Var
		, eRedefined		:: Var }

	-- | Bindings for the same function have different numbers of arguments
	| ErrorBindingAirity
		{ eVar1			:: Var
		, eAirity1		:: Int
		, eVar2			:: Var
		, eAirity2		:: Int }

	-- | Tried to make an instance for a method that's not in the specified class.
	| ErrorNotMethodOfClass
		{ eInstVar		:: Var
		, eClassVar		:: Var }
		
	-- | Type signature quantifies a material variable.
	| ErrorQuantifiedMaterialVar	
		{ eVarSig		:: Var
		, eTypeSig		:: Type 
		, eVarQuantified	:: Var }

	-- | Type signature quantifies a dangerous variable.
	| ErrorQuantifiedDangerousVar
		{ eVar			:: Var }

	deriving Show		


-- Pretty Printer ---------------------------------------------------------------------------------
-- | Pretty printer for error messages
instance Pretty Error PMode where

 -- Parser Errors ------------------------------------------------------
 ppr (ErrorLayoutLeft tok)
 	= ppr $ unlines
	[ prettyTokenPos tok
	, "    Layout error: Nested block starts to the left of the enclosing one."]

 ppr (ErrorLayoutNoBraceMatch tok)
 	= ppr $ unlines
	[ prettyTokenPos tok
	, "    Layout error: Explicit close brace must match an explicit open brace."]
 
 ppr (ErrorLexicalStringTabs tok)
	= ppr $ unlines
	[ prettyTokenPos tok
	, "    Lexer error: Literal string contains tab characters."]

 ppr (ErrorLexicalEscape tok)
	= ppr $ unlines
	[ prettyTokenPos tok
	, "    Lexer error: Unhandled escape sequence." ]

 ppr (ErrorParse tok str)
 	= ppr
	$ unlines $
	[ prettyTokenPos tok
	, "    Parse error: " ++ str ]

 ppr (ErrorParsePos sp str)
 	= ppr
	$ unlines $
	[ pprStrPlain sp
	, "    Parse error: " ++ str ]

 ppr (ErrorParseBefore tt)
	| Just toks	<- sequence 
			$ map takeToken 
			$ take 10 tt

 	= let	Just t1	= takeHead tt
 	  in	ppr $ unlines $ 
		[ prettyTokenPos t1
		, "    Parse error before: " ++ (catInt " " $ map Token.showSource toks) ++ " ..."]
		
	| otherwise
	= ppr "    Parse error at start of module"

 ppr (ErrorParseEnd)
 	= ppr $ unlines $ 
	[ "    Parse error at end of input.\n" ]


 -- Defixer Errors -----------------------------------------------------
 ppr (ErrorDefixNonAssoc vs)
  = let	Just v	= takeHead vs
    in	prettyPos v % "\n"
	% "    Precedence parsing error.\n"
	% "      Cannot have multiple, adjacent, non-associative operators of the\n"
	% "      same precedence in an infix expression.\n"
	% "\n"
	% "      Offending operators: " % ", " %!% (map varName vs) % "\n"

 ppr (ErrorDefixMixedAssoc vs)
  = let	Just v	= takeHead vs
    in	prettyPos v % "\n"
	% "    Precedence parsing error.\n"
	% "      Cannot have operators of same precedence but with differing\n"
	% "      associativities in an infix expression.\n"
	% "\n"
	% "      Offending operators: " % ", " %!% (map varName vs) % "\n"


 -- Renamer Errors ------------------------------------------------------------
 ppr err@(ErrorUndefinedVar{})
	= prettyPos (eUndefined err)								% "\n"
	% "     Undefined " 
		% (shortNameOfSpace $ varNameSpace (eUndefined err))
		% " variable '" % eUndefined err % "'.\n"

 ppr err@(ErrorShadowForall{})
	= prettyPos (eShadowVar err)								% "\n"
	% "     Shadowed type variable '" % eShadowVar err  
	% "' in 'forall' quantifier.\n"

 ppr err@(ErrorRedefinedVar{})
	= prettyPos (eRedefined err)								% "\n"
	% "     Redefined "
		% thing % " '"
		% varName (eRedefined err) % "'\n"
	% "      first defined at: " 	% prettyPos (eFirstDefined err) 			% "\n"

	where var = eFirstDefined err
	      thing 
		| isCtorName var
		, varNameSpace var == NameValue
		= ppr "data constructor"

		| isCtorName var
		, varNameSpace var == NameType
		= ppr "type constructor"
		
		| otherwise
		= (shortNameOfSpace $ varNameSpace var) % " variable"

 ppr err@(ErrorAmbiguousVar{})
	= prettyPos (eBoundVar err)								% "\n"
	% "     Ambiguous occurrence of '" % eBoundVar err % "'\n"
	% "      could be any of: " % ", " %!% map ppr (eBindingVars err)			% "\n"


 -- Lint Errors ---------------------------------------------------------------
 ppr err@(ErrorSigLacksBinding{})
	= prettyPos (eSigVar err)								% "\n"
	% "     Type signature for '" 	% varWithoutModuleId (eSigVar err) 
					% "' lacks accompanying binding."			% "\n"


 -- Desugarer Errors ----------------------------------------------------------
 ppr err@(ErrorProjectRedefDataField{})
	= prettyPos (eRedefined err)								% "\n"
	% "     Projection '"	% eFirstDefined err % "' over data type '" % eDataVar err 	% "'\n"
	% "      collides with a field name at: "	% prettyPos (eFirstDefined err)		% "\n"

 ppr err@(ErrorRedefClassInst{})
	= prettyPos (eRedefined err)								% "\n"
	% "     Instance '"	% varName (eFirstDefined err)
	%						"' of class '" % varName (eClass err) 	% "'\n"
	% "      first defined at: "	% prettyPos (eFirstDefined err) 			% "\n"
	% "      redefined at    : "	% prettyPos (eRedefined err)				% "'\n"

 ppr (ErrorBindingAirity var1 airity1 var2 airity2)
 	= prettyPos var1 % "\n"
	% "    Bindings for '" % var1 % "' have a differing number of arguments.\n"
	% "\n"
	% "    binding at " % prettyPos var1 % " has " % airity1 % " arguments\n"
	% "           but " % prettyPos var2 % " has " % airity2 % "\n"
	
 ppr (ErrorNotMethodOfClass vInst vClass)
	= prettyPos vInst % "\n"
	% "    '" % vInst % "' is not a (visible) method of class '" % vClass % "'.\n"

 ppr (ErrorQuantifiedMaterialVar vSig tSig vQuant)
  	= varErr vQuant
	[ "    Variable '" 
			% varWithoutModuleId vQuant 
			% "' cannot be quantified because it is material in this type."
	, ppr "    Offending signature:"
	, blank
	, indent $ ppr $ prettyVTS vSig tSig ]


 ppr (ErrorQuantifiedDangerousVar v)
  	= vcat
	[ ppr $ prettyPos v
	, "    Variable '" % v % "' cannot be quantified because it is dangerous in this type." ]
	

-- Utils ------------------------------------------------------------------------------------------	
prettyVTS v t
 	= indentSpace 4
	$ "    " 
		++ varName v 
		++ "\n  :: "
		++ (indentSpace 2 $ pprStrPlain $ prettyTypeSplit $ t)

varErr var ls
	= prettyPos var % newline
	% (indent $ vcat ls)




