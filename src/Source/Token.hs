
-- | Source tokens
module Source.Token
	( Token  (..)	-- The tokens.
	, TokenP (..)	-- Tokens with file position info attached.
	, prettyTokenPos
	, takeToken)

where

import Util

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

takeToken tt
	| TokenP { token = t }	<- tt
	= Just t
	
	| otherwise
	= Nothing


-- | just show the token instead of the whole thing
instance Show TokenP where
 show tok	
   = case tok of
   	TokenP { token = t }	-> show t
	StartBlock i		-> "StartBlock " ++ show i
	StartLine  i		-> "StartLine " ++ show i

instance Pretty TokenP where
 ppr	= ppr . show 

-- | pretty print the position of this token
prettyTokenPos :: TokenP	-> String
prettyTokenPos tt
 = case tt of
 	TokenP	{ tokenFile	= file
		, tokenLine	= line
		, tokenColumn	= column }

	 -> file ++ ":" ++ (show $ line )
		 ++ ":" ++ (show $ (column - 1))	-- make columns start at 0

	_ -> "unknown"

-- | Source tokens
data Token 

	-- Variables -----------------------------------------------------------
	= ModuleName	[String]	-- a qualified module name, broken into parts
	| Var		String		-- a non-constructor variable
	| Con		String		-- a constructor variable
	| Symbol	String		-- a symbol

	-- Literal values ------------------------------------------------------
	| CInt		Int
	| CChar		Char
	| CFloat	Float
	| CString	String
	

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

	-- Some other junk not recognised by the lexer.
	-- 	The junk token will cause a parse error.
	| Junk 		String
	deriving (Show, Eq)




	
