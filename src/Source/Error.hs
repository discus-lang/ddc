
module Source.Error
(
	Error(..)
)

where

-----
import Util
import Util.Pretty

import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))
import Shared.VarUtil		(prettyPos, prettyPosBound)
import Shared.VarSpace		(spaceName)
import Shared.Error

import qualified Source.Token	as Token
import qualified Source.TokenShow as Token
import Source.Token	(Token(..), TokenP(..))

import Source.Exp
import Source.Pretty

stage	= "Source.Error"


data Error
	= ErrorParse		TokenP String
	| ErrorParseBefore	TokenP
	| ErrorParseEnd		
	| ErrorUndefinedVar	
	{ eUndefined		:: Var }

	| ErrorShadowVar
	{ eShadowVar		:: Var }

	| ErrorRedefinedVar	
	{ eFirstDefined		:: Var
	, eRedefined		:: Var }


	| ErrorDefixNonAssoc	[Var]
	| ErrorDefixMixedAssoc	[Var]

	deriving Show		



instance Pretty Error where

 pretty (ErrorParse tok str)
 	= unlines $
	[ prettyPosT tok
	, "    Parse error: " ++ str ]

 pretty (ErrorParseBefore tok)
 	= unlines $ 
	[ prettyPosT tok
	, "    Parse error before: " ++ prettyTok tok ++ "."]

 pretty (ErrorParseEnd)
 	= unlines $ 
	[ "    Parse error at end of input.\n" ]

 pretty err@(ErrorUndefinedVar{})
 	= pretty 	
	$ prettyPos (eUndefined err)								% "\n"
	% "     Undefined variable '" 	% eUndefined err  
	% "' in namespace " 		% (spaceName $ Var.nameSpace (eUndefined err))		% ".\n"

 pretty err@(ErrorShadowVar{})
 	= pretty 	
	$ prettyPos (eShadowVar err)								% "\n"
	% "     Shadowed REC variable '" % eShadowVar err  
	% "' in namespace " 		% (spaceName $ Var.nameSpace (eShadowVar err))		% ".\n"
	
 pretty err@(ErrorRedefinedVar{})
 	= pretty
	$ prettyPos (eRedefined err)								% "\n"
	% "     Redefined variable '" 	% eRedefined err 					% "'\n"
	% "          in namespace: " 	% (spaceName $ Var.nameSpace (eFirstDefined err))	% "\n"
	% "      first defined at: " 	% prettyPos (eFirstDefined err) 			% "\n"

 pretty (ErrorDefixNonAssoc (v:vs))
	= pretty
	$ prettyPos v % "\n"
	% "    Precedence parsing error.\n"
	% "      Cannot have multiple non-associative operators of the same precedence\n"
	% "      in an infix expression.\n"
	% "\n"
	% "      Offending operators: " % ", " %!% (map Var.name (v:vs)) % "\n"

 pretty (ErrorDefixMixedAssoc (v:vs))
 	= pretty
	$ prettyPos v % "\n"
	% "    Precedence parsing error.\n"
	% "      Cannot have operators of same precedence but with differing\n"
	% "      associativities in an infix expression.\n"
	% "\n"
	% "      Offending operators: " % ", " %!% (map Var.name (v:vs)) % "\n"
 
 pretty x
  	= panic stage
	$ "pretty: no match for " % show x
	

prettyPosT :: TokenP	-> String
prettyPosT tok
	=         (Token.file tok)
	++ ":" ++ (show $ Token.line tok) 
	++ ":" ++ (show $ Token.column tok)

prettyTok tok
	= "'" ++ (Token.showSource $ Token.token tok) ++ "'"


