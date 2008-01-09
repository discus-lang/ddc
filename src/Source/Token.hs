-----
-- Source.Token
--
-- Summary:
--	Token definitions.
--
--

module Source.Token
(
	Token (..),	-- The tokens.
	TokenP (..)	-- Tokens with file position info attached.
)

where

data Token 
	
	-- Keywords, these have a special meaning in all contexts
	-- and cannot be used as regular variables.
	= Pragma		-- ^ pragma
	
	| Foreign		-- ^ foreign
	| Import		-- ^ import
	| Export		-- ^ export	

	| Module		-- ^ module		(weak)
	| Elaborate		-- ^ elaborate		(weak)
	| Const			-- ^ const		(weak)
	| Mutable		-- ^ mutable		(weak)
	| Extern		-- ^ extern		(weak)
	| CCall			-- ^ ccall		(weak)
	
	| Data			-- ^ data
	| Region		-- ^ region
	| Effect		-- ^ effect

	| Class			-- ^ class
	| Instance		-- ^ instance
	| Project		-- ^ project

	| InfixR		-- ^ infixr
	| InfixL		-- ^ infixl
	| Infix			-- ^ infix

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
	
	| Forall		-- ^ forall

	-- 
	| HasType		-- ^ ::
	| HasTypeQuant		-- ^ :*

	| HasTypeExact		-- ^ :::
	| HasTypeExactQuant	-- ^ ::*

	| IsSubtypeOf		-- ^ <:
	| IsSubtypeOfQuant	-- ^ <*

	| HasOpType		-- ^ :$
	| HasConstraint		-- ^ :-

	| RightArrow		-- ^ ->
	| LeftArrow		-- ^ <-
	| LeftArrowLazy		-- ^ <\@-

	| GuardCase		-- ^ |-
	| GuardCaseC		-- ^ ,-

	| GuardUnboxed		-- ^ |#
	| GuardUnboxedC		-- ^ ,#

	| GuardDefault		-- ^ \=

	| Unit			-- ^ ()
	| DotDot		-- ^ ..

	| Hash			-- ^ #
	| Star			-- ^ *
	| Dash			-- ^ -
	| Plus			-- ^ +
	| Percent		-- ^ %
	| At			-- ^ \@
	| Bang			-- 
	| FSlash		-- 
	| Dollar		-- 
	| Tilde			-- 
	| Underscore		-- ^ _
	| Hat			-- ^ ^

	-- Parenthesis.
	| ABra			-- ^ < (angled)
	| AKet			-- ^ >

	| CBra			-- ^ { (curley)
	| CKet			-- ^ }

	| RBra			-- ^ ( (round)
	| RKet			-- ^ )

	| SBra			-- ^ \[ (square)
	| SKet			-- ^ ]

	| BSlash		-- ^ \
	| BTick			-- ^ `
	| Equals		-- ^ =
	| Comma			-- ^ ,
	| Colon			-- ^ :
	| SColon		-- ^ ;
	| Bar			-- ^ |
	| Dot			-- ^ .
	| And			-- ^ &
	
	| ModuleName	[String]	-- module qualifier / name
					--	broken into parts
	| Var		String
	| Con		String
	| Symbol	String
	
	-- Constants.
	| CInt		Int
	| CChar		Char
	| CFloat	Float
	| CString	String

	-- These tokens get eaten up by Source.Lexer.scan.
	| NewLine			
	| CommentLineStart
	| CommentBlockStart
	| CommentBlockEnd

	-- Some other junk not recognised by the lexer.
	-- 	The junk token will cause a parse error.
	| Junk 		String

	deriving (Show, Eq)

data TokenP = 
     TokenP 
     	{ token 	:: Token
	, file		:: String
	, line		:: Int
	, column	:: Int }
	
	deriving Eq

instance Show TokenP where
 show tok	= show $ token tok


	
