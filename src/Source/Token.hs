
-- | Source tokens
module Source.Token
	( Token  (..)	-- The tokens.
	, TokenP (..)	-- Tokens with file position info attached.
	, prettyTokenPos
	, takeToken
	, liftToken
	, tokenPHasStringTabs
	, tokenPSetFileName
	, expandEscapedChar
	, expandEscapedChars)
where
import Shared.Pretty
import Shared.Literal
import Data.Char


-- | Wraps up a token with its position in the source file
data TokenP 
	= TokenP 
	     	{ token		:: Token	-- the token
		, tokenFile	:: String	-- the file this token was from
		, tokenLine	:: Int		-- line number in the file
		, tokenColumn	:: Int }	-- column number in the file

	-- Offside -------------------------------------------------------------
	-- extra tokens to help perform the offside rule
	| StartBlock	Int	-- signals the start of a block at this column
	| StartLine	Int	-- signals the start of a line at this column

	deriving Eq


-- | Take the token from a TokenP.
takeToken :: TokenP -> Maybe Token
takeToken tt
	| TokenP { token = t }	<- tt	= Just t
	| otherwise			= Nothing

-- | Apply a token transform function to the token in a tokenp
liftToken :: (Token -> Token) -> TokenP -> TokenP
liftToken f tokp	= tokp { token = f (token tokp) } 


-- | Shows the token inside a TokenP.
instance Show TokenP where
 show tok	
   = case tok of
   	TokenP { token = t }	-> show t
	StartBlock i		-> "StartBlock " ++ show i
	StartLine  i		-> "StartLine " ++ show i

instance Pretty TokenP PMode where
 ppr	= ppr . show 


-- | Pretty print the position of this token.
prettyTokenPos :: TokenP	-> String
prettyTokenPos tt
 = case tt of
 	TokenP	{ tokenFile	= file
		, tokenLine	= line
		, tokenColumn	= column }

	 -> file ++ ":" ++ (show $ line )
		 ++ ":" ++ (show $ (column - 1))	-- make columns start at 0

	_ -> "unknown"


-- | Check if this token represents a String that has tabs in it.
tokenPHasStringTabs :: TokenP -> Bool
tokenPHasStringTabs tt
 = case tt of
	TokenP { token = Literal (LiteralFmt (LString str) _) }
	 | elem '\t' str	-> True

	_			-> False

tokenPSetFileName :: String -> TokenP -> TokenP
tokenPSetFileName name token
 = case token of
 	TokenP{}	-> token { tokenFile = name }
	_		-> token



-- | Convert '\n \b' etc sugar to real characters
-- expandStringEscapes :: String -> String
expandEscapedChars :: String -> Maybe String
expandEscapedChars str
 = case expandEscapedChar str of
	Nothing			-> Nothing
	Just ([], [])		-> Just []

	Just (front1, rest)		
	 -> case expandEscapedChars rest of
		Nothing		-> Nothing
		Just rest'	-> Just (front1 ++ rest')


expandEscapedChar :: String -> Maybe (String, String)
expandEscapedChar str
 = case str of
	('\\': 'b'   : rest)	-> Just ([chr 0x08], rest)
	('\\': 't'   : rest)	-> Just ([chr 0x09], rest)
	('\\': 'n'   : rest)	-> Just ([chr 0x0a], rest)
	('\\': 'v'   : rest)	-> Just ([chr 0x0b], rest)
	('\\': 'f'   : rest)	-> Just ([chr 0x0c], rest)
	('\\': 'r'   : rest)	-> Just ([chr 0x0d], rest)
	('\\': '\\'  : rest)	-> Just ([chr 0x5c], rest)
	('\\': '\''  : rest)	-> Just ([chr 0x27], rest)
	('\\': '\"'  : rest)	-> Just ([chr 0x22], rest)
	('\\': _     : rest)	-> Nothing

	(x:rest)		-> Just ([x], rest) 

	[]			-> Just ([], [])


-- | Source tokens
data Token 

	-- Variables -----------------------------------------------------------
	= ModuleName	[String]	-- a qualified module name, broken into parts
	| Var		String		-- a non-constructor variable
	| VarField	String		-- a non-constructor variable which a field of the opened object ie (_var)
	| Con		String		-- a constructor variable
	| Symbol	String		-- a symbol

	-- Literal values
	| Literal	LiteralFmt
		
	-- Weak keywords -------------------------------------------------------
	-- They only have special meaning in particular contexts, in others contexts
	-- they should be converted back to regular variables.
	| Module		-- ^ module
	| Elaborate		-- ^ elaborate
	| Const			-- ^ const
	| Mutable		-- ^ mutable
	| Extern		-- ^ extern
	| CCall			-- ^ ccall
	

	-- Regular keywords ----------------------------------------------------
	| Pragma		-- ^ pragma

	-- infix definitions
	| InfixR		-- ^ infixr
	| InfixL		-- ^ infixl
	| Infix			-- ^ infix

	-- module definitions
	| Foreign		-- ^ foreign
	| Import		-- ^ import
	| Export		-- ^ export	
	
	-- type definitions
	| Type			-- ^ type
	| Data			-- ^ data
	| Region		-- ^ region
	| Effect		-- ^ effect
	| Class			-- ^ class
	| Instance		-- ^ instance
	| Project		-- ^ project
	| Forall		-- ^ forall

	-- expresions
	| Let			-- ^ let
	| In			-- ^ in
	| Where			-- ^ where

	| Case			-- ^ case
	| Of			-- ^ of
	| Match			-- ^ match

	| If			-- ^ if
	| Then			-- ^ then
	| Else			-- ^ else

	| Throw			-- ^ throw
	| Try			-- ^ try
	| Catch			-- ^ catch
	| With			-- ^ with

	| Do			-- ^ do
	| While			-- ^ while
	| When			-- ^ when
	| Unless		-- ^ unless
	| Break			-- ^ break
	

	-- Symbols ---------------------------------------------------------------------------------

	-- type symbols
	| HasType		-- ^ ::
	| IsSubtypeOf		-- ^ <:
	| IsSuptypeOf		-- ^ :>

	| HasOpType		-- ^ :$
	| HasConstraint		-- ^ :-

	| RightArrow		-- ^ ->
	| RightArrowEquals	-- ^ =>
	| HoldsMono		-- ^ $>

	-- shared between types and expressions
	| LeftArrow		-- ^ <-
	| Unit			-- ^ ()

	-- expression symbols
	| LeftArrowLazy		-- ^ <\@-

	| GuardCase		-- ^ |-
	| GuardCaseC		-- ^ ,-
	| GuardUnboxed		-- ^ |#
	| GuardUnboxedC		-- ^ ,#
	| GuardDefault		-- ^ \=
	
	| BackSlashDot		-- ^ \.
	
	| Dot			-- ^ .
	| DotDot		-- ^ ..

	| Hash			-- ^ #
	| Star			-- ^ *
	| Dash			-- ^ -
	| Plus			-- ^ +
	| Percent		-- ^ %
	| At			-- ^ \@
	| Bang			-- 
	| Dollar		-- 
	| Tilde			-- 
	| Underscore		-- ^ _
	| Hat			-- ^ ^
	| ForwardSlash		-- 
	| BackSlash		-- ^ \
	| BackTick		-- ^ `
	| Equals		-- ^ =
	| Comma			-- ^ ,
	| Colon			-- ^ :
	| SemiColon		-- ^ ;
	| Bar			-- ^ |
	| And			-- ^ &

	-- parenthesis ---------------------------------------------------------
	| ABra			-- ^ < (angled)
	| AKet			-- ^ >

	| CBra			-- ^ { (curley)
	| CKet			-- ^ }

	| RBra			-- ^ ( (round)
	| RKet			-- ^ )

	| SBra			-- ^ \[ (square)
	| SKet			-- ^ ]
	
	-- Comments ------------------------------------------------------------
	-- These get eaten up before parsing
	| NewLine			
	| CommentLineStart
	| CommentBlockStart
	| CommentBlockEnd

	| CommentPragma	String

	-- Some other junk not recognised by the lexer.
	-- 	The junk token will cause a parse error.
	| Junk 		String
	deriving (Show, Eq)




	
