
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

 ppr (ErrorParse tok str)
 	= ppr
	$ unlines $
	[ prettyPosT tok
	, "    Parse error: " ++ str ]

 ppr (ErrorParseBefore tok)
 	= ppr
	$ unlines $ 
	[ prettyPosT tok
	, "    Parse error before: " ++ prettyTok tok ++ "."]

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
	

prettyPosT :: TokenP	-> String
prettyPosT tok
	=         (Token.file tok)
	++ ":" ++ (show $ Token.line tok) 
	++ ":" ++ (show $ Token.column tok)

prettyTok tok
	= "'" ++ (Token.showSource $ Token.token tok) ++ "'"


