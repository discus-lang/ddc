
-- Pretty printing of parse and renamer errors in the source program.
module Source.Error
	( Error(..))
where

-----
import Util
import Util.Pretty

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))
import Shared.VarUtil		(prettyPos, prettyPosBound)
import Shared.VarSpace		(spaceName)
import Shared.Error
import Shared.Base

import qualified Source.Token	as Token
import qualified Source.TokenShow as Token
import Source.Token

import Source.Exp
import Source.Pretty

stage	= "Source.Error"


-- | All the errors that the parser and renamer can generate
data Error
	= ErrorLayoutLeft		TokenP
	| ErrorLayoutNoBraceMatch 	TokenP
	| ErrorParse			TokenP String
	| ErrorParsePos			SourcePos String
	| ErrorParseBefore		[TokenP]
	| ErrorParseEnd		
	| ErrorUndefinedVar	
		{ eUndefined		:: Var }

	| ErrorShadowVar
		{ eShadowVar		:: Var }

	| ErrorRedefinedVar	
		{ eFirstDefined		:: Var
		, eRedefined		:: Var }

	| ErrorDefixNonAssoc		[Var]
	| ErrorDefixMixedAssoc		[Var]


	deriving Show		


-- | Pretty printer for error messages
instance Pretty Error where

 ppr (ErrorLayoutLeft tok)
 	= ppr $ unlines
	[ prettyTokenPos tok
	, "    Layout error: Nested block starts to the left of the enclosing one."]
		
 ppr (ErrorLayoutNoBraceMatch tok)
 	= ppr $ unlines
	[ prettyTokenPos tok
	, "    Layout error: Explicit close brace must match an explicit open brace."]

 ppr (ErrorParse tok str)
 	= ppr
	$ unlines $
	[ prettyTokenPos tok
	, "    Parse error: " ++ str ]

 ppr (ErrorParsePos sp str)
 	= ppr
	$ unlines $
	[ pprStr sp
	, "    Parse error: " ++ str ]

 ppr (ErrorParseBefore tt@(t1 : _))
	| Just toks	<- sequence 
			$ map takeToken 
			$ take 10 tt

 	= ppr $ unlines $ 
	[ prettyTokenPos t1
	, "    Parse error before: " ++ (catInt " " $ map Token.showSource toks) ++ " ..."]



 ppr (ErrorParseEnd)
 	= ppr $ unlines $ 
	[ "    Parse error at end of input.\n" ]

 ppr err@(ErrorUndefinedVar{})
	= prettyPos (eUndefined err)								% "\n"
	% "     Undefined variable '" 	% eUndefined err  
	% "' in namespace " 		% (spaceName $ Var.nameSpace (eUndefined err))		% ".\n"

 ppr err@(ErrorShadowVar{})
	= prettyPos (eShadowVar err)								% "\n"
	% "     Shadowed TREC variable '" % eShadowVar err  
	% "' in namespace " 		% (spaceName $ Var.nameSpace (eShadowVar err))		% ".\n"
	
 ppr err@(ErrorRedefinedVar{})
	= prettyPos (eRedefined err)								% "\n"
	% "     Redefined variable '" 	% eRedefined err 					% "'\n"
	% "          in namespace: " 	% (spaceName $ Var.nameSpace (eFirstDefined err))	% "\n"
	% "      first defined at: " 	% prettyPos (eFirstDefined err) 			% "\n"

 ppr (ErrorDefixNonAssoc (v:vs))
	= prettyPos v % "\n"
	% "    Precedence parsing error.\n"
	% "      Cannot have multiple non-associative operators of the same precedence\n"
	% "      in an infix expression.\n"
	% "\n"
	% "      Offending operators: " % ", " %!% (map Var.name (v:vs)) % "\n"

 ppr (ErrorDefixMixedAssoc (v:vs))
	= prettyPos v % "\n"
	% "    Precedence parsing error.\n"
	% "      Cannot have operators of same precedence but with differing\n"
	% "      associativities in an infix expression.\n"
	% "\n"
	% "      Offending operators: " % ", " %!% (map Var.name (v:vs)) % "\n"
 
 ppr x
  	= panic stage
	$ "ppr: no match for " % show x
	



