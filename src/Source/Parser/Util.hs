
module Source.Parser.Util
	( Var		-- from Shared.Var
	, (<|>)		-- from Parsec

	, SP, Parser
	, toVar
	, makeVar
	, vNameN
	, vNameV, vNameT, vNameR, vNameE, vNameC, vNameW, vNameF
	, spV, spTP, spX, spW
	, makeParsecSourcePos)

where

import Source.Util
import Source.Exp
import qualified Source.Token 	as K

import qualified Shared.Var	as Var
import Shared.Var		(Var)
import Shared.VarSpace		(NameSpace(..))
import Shared.Error
import Shared.Base

import qualified Text.ParserCombinators.Parsec.Prim	as Parsec
import qualified Text.ParserCombinators.Parsec.Pos	as Parsec
import qualified Text.ParserCombinators.Parsec.Prim	as Parsec

import Text.ParserCombinators.Parsec.Prim		( (<|>) )


import Util

stage	= "Source.Parser.Util"

----
type SP		= SourcePos
type Parser a 	= Parsec.GenParser K.TokenP () a

-- Conversion --------------------------------------------------------------------------------------
-- | Convert a token to a variable
--	We need to undo the lexer tokens here because some of our 
--	"reserved symbols" alias with valid variable names.
--
toVar :: K.TokenP -> Var
toVar	 tok
 = case K.token tok of
	K.Var    	name	-> Var.loadSpaceQualifier $ makeVar name tok
	K.VarField	name	-> vNameF $ makeVar name tok
	K.Con		name	-> Var.loadSpaceQualifier $ makeVar name tok
	K.Symbol 	name	-> makeVar name tok
	_ -> case lookup (K.token tok) toVar_table of
		Just name	-> makeVar name tok
		Nothing		-> panic stage ("toVar: bad token: " ++ show tok)


-- | String representations for these tokens.
toVar_table :: [(K.Token, String)]
toVar_table = 
	[ (K.Colon,		":")
	, (K.Star,		"*")
	, (K.Dash,		"-")
	, (K.At,		"@")
	, (K.Hash,		"#") 
	, (K.ABra,		"<")
	, (K.AKet,		">") 
	, (K.ForwardSlash,	"/")
	, (K.Plus,		"+")
	, (K.Dot,		".")
	, (K.Dollar,		"$")
	, (K.Tilde,		"~")
	, (K.Percent,		"%") ]

-- | Make a variable with this name,
--	using the token as the source location for the var.
makeVar :: String -> K.TokenP -> Var
makeVar    name@(n:_) tok
 = let 	sp	= SourcePos (K.tokenFile tok, K.tokenLine tok, K.tokenColumn tok)
   in	(Var.new name)
	 	{ Var.info	= [ Var.ISourcePos sp ] }


-- | Force the namespace of this variable
--	If it has already been set differently then panic
vNameN :: NameSpace -> Var -> Var
vNameN space v
	-- var has no namespace, so give it one
	| Var.nameSpace v == NameNothing
	= v { Var.nameSpace = space }

	-- var had a different namespace, oh oh.
	| Var.nameSpace v /= space
	= panic stage
	$ "vNameN: conflicting namespace for variable " % v	% "\n"
	% "   name space was     " % Var.nameSpace v		% "\n"
	% "   tried to set it to " % space			% "\n"
	
	-- var already has the right namespace
	| otherwise
	= v 

vNameV		= vNameN NameValue
vNameT		= vNameN NameType
vNameR		= vNameN NameRegion
vNameE		= vNameN NameEffect
vNameC		= vNameN NameClosure
vNameW		= vNameN NameClass
vNameF		= vNameN NameField



-- | Slurp the source position from this token.
spTP :: K.TokenP -> SP
spTP    tok
	= SourcePos (K.tokenFile tok, K.tokenLine tok, K.tokenColumn tok)


-- | Slurp the source position from this expression.
spX :: Exp SP -> SP
spX 	= sourcePosX


-- | Slurp the source position from this pattern.
spW	= sourcePosW

-- | Slurp the source position from this variable.
spV :: Var -> SP
spV var
 = let	[sp]	= [sp | Var.ISourcePos sp <- Var.info var]
   in	sp


makeParsecSourcePos :: K.TokenP -> Parsec.SourcePos
makeParsecSourcePos tok
	= (flip Parsec.setSourceColumn) (K.tokenColumn tok)
	$ (flip Parsec.setSourceLine)   (K.tokenLine tok)
	$ Parsec.initialPos      	(K.tokenFile tok)
