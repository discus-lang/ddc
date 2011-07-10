{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- Pretty printing of parse and renamer errors in the source program.
module DDC.Source.Error
	(Error(..))
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

	-- | Source file was either empty or only contained comments.
	| ErrorParseNoCode

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
 	= tokErr tok
	$ "Layout error: Nested block starts to the left of the enclosing one."

 ppr (ErrorLayoutNoBraceMatch tok)
 	= tokErr tok
	$ "Layout error: Explicit close brace must match an explicit open brace."
 
 ppr (ErrorLexicalStringTabs tok)
	= tokErr tok
	$ "Lexer error: Literal string contains tab characters."

 ppr (ErrorLexicalEscape tok)
	= tokErr tok
	$ "Lexer error: Unhandled escape sequence."

 ppr (ErrorParse tok str)
 	= tokErr tok
	$ "Parse error: " % str

 ppr (ErrorParseNoCode)
 	= ppr $ indent "Parse error: Source file contains no code."

 ppr (ErrorParsePos sp str)
 	= spErr sp
	$ "Parse error: " % str

 ppr (ErrorParseBefore tt)
	| Just toks	<- sequence 
			$ map takeToken 
			$ take 10 tt

 	= let	Just t1	= takeHead tt
 	  in	tokErr t1 
		 $ "Parse error before: " % hsep (map Token.showSource toks) % " ..."
		
	| otherwise
	= ppr $ indent "Parse error at start of module"

 ppr (ErrorParseEnd)
 	= ppr $ indent "Parse error at end of input."


 -- Defixer Errors -----------------------------------------------------
 ppr (ErrorDefixNonAssoc vs)
  = let	Just v	= takeHead vs
    in	varErr v
	 $  "Precedence parsing error."
	 %! blank
	 %! "Cannot have multiple, adjacent, non-associative operators of the"
	 %! "same precedence in an infix expression."
	 %! blank
	 %! "Offending operators: " % punc ", " (map varName vs)

 ppr (ErrorDefixMixedAssoc vs)
  = let	Just v	= takeHead vs
    in	varErr v
	 $  "Precedence parsing error."
	 %! blank
	 %! "Cannot have operators of same precedence but with differing"
	 %! "associativities in an infix expression."
	 %! blank
	 %! "Offending operators: " % punc "," (map varName vs)


 -- Renamer Errors ------------------------------------------------------------
 ppr err@(ErrorUndefinedVar{})
	= varErr (eUndefined err)
	$ "Undefined" 	%% (shortNameOfSpace $ varNameSpace (eUndefined err))
			%% "variable" %% quotes (eUndefined err) % "."

 ppr err@(ErrorShadowForall{})
	= varErr (eShadowVar err)
	$ "Shadowed type variable" 
			%% quotes (eShadowVar err)
			%% "in forall quantifier."

 ppr err@(ErrorRedefinedVar{})
	= varErr (eRedefined err)
	$  "Redefined" %% thing 	%% quotes (varName (eRedefined err))
	%! "    first defined at:"	%% prettyPos (eFirstDefined err)

	where var = eFirstDefined err
	      thing 
		| isCtorName var
		, varNameSpace var == NameValue
		= ppr "data constructor"

		| isCtorName var
		, varNameSpace var == NameType
		= ppr "type constructor"
		
		| otherwise
		= (shortNameOfSpace $ varNameSpace var) %% "variable"

 ppr err@(ErrorAmbiguousVar{})
	= varErr (eBoundVar err)
	$ "Ambiguous occurrence of" %% quotes (eBoundVar err)
	% "    could be any of: " % punc ", " (eBindingVars err)


 -- Lint Errors ---------------------------------------------------------------
 ppr err@(ErrorSigLacksBinding{})
	= varErr (eSigVar err)
	$  "Type signature for" %% quotes (varWithoutModuleId (eSigVar err))
	%% "lacks accompanying binding."


 -- Desugarer Errors ----------------------------------------------------------
 ppr err@(ErrorProjectRedefDataField{})
	= varErr (eRedefined err)
	$  "Projection"	%% quotes (eFirstDefined err) 
			%% "over data type" %% quotes (eDataVar err)
	%! "    collides with a field name at: " % prettyPos (eFirstDefined err)

 ppr err@(ErrorRedefClassInst{})
	= varErr (eRedefined err)
	$  "Duplicate instance "
			%% quotes (varName (eClass err))
			%% quotes (varName (eFirstDefined err))
	%! blank
	%! "    first defined at: " % prettyPos (eFirstDefined err) 
	%! "     is redefined at: " % prettyPos (eRedefined err)

 ppr (ErrorBindingAirity var1 airity1 var2 airity2)
 	= varErr var1 
	$  "Bindings for" %% quotes var1 %% "have a differing number of arguments."
	%! blank
	%! "  the binding at:" %% prettyPos var1
	%! "             has:" %% airity1 %% "arguments"
	%! blank
	%! "             but:" %% prettyPos var2
	%! "             has:" %% airity2
	
 ppr (ErrorNotMethodOfClass vInst vClass)
	= varErr vInst
	$ quotes vInst %% "is not a (visible) method of class" %% quotes vClass % "."

 ppr (ErrorQuantifiedMaterialVar vSig tSig vQuant)
  	= varErr vQuant
	$  "Variable" 	%% quotes (varWithoutModuleId vQuant)
			%% "cannot be quantified because it is material in this type."
	%! blank
	%! "Offending signature:"
	%! prettyVT vSig tSig

 ppr (ErrorQuantifiedDangerousVar vDanger)
  	= varErr vDanger
	$ "Variable" %% quotes vDanger %% "cannot be quantified because it is dangerous in this type."
	

-- Utils ------------------------------------------------------------------------------------------	
prettyVT v t
 	= "    " % varName v % nl
	%> ("::" %% prettyTypeSplit t)


varErr var str
	= prettyPos var % newline
	% indent str

tokErr tok str
	= prettyTokenPos tok % newline
	% indent str

spErr sp str
	= sp % newline % indent str


