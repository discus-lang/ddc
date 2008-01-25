
{ 
module Source.Lexer
	( scan 
	, showSource
	, sourceTokens )

where

import Source.Token
import Source.TokenShow
import Source.Error
import Shared.Error
import Util
import Data.Char
import Debug.Trace

}

%wrapper "posn"

$digit	= 0-9

$upper	= [A-Z]
$lower  = [a-z]
$alpha	= [$lower $upper]

-- The symbols that can appear in a var
$varsym	= [\' \_]


-- $ssym	= [\\ \= \: \|]
$sym	= [\. \! \# \$ \% \& \* \+ \/ \< \> \? \@ \^ \- \~ \\ \= \: \|]


tokens :-
 $white # \n		;

 \n			{ ptag NewLine			}
 \- \- \-*		{ ptag CommentLineStart		}
 \{ \-			{ ptag CommentBlockStart	}
 \- \}			{ ptag CommentBlockEnd		}

 pragma			{ ptag Pragma			}

 foreign		{ ptag Foreign			}
 import			{ ptag Import			}
 export			{ ptag Export			}

 module			{ ptag Module			} 
 elaborate		{ ptag Elaborate		}
 const			{ ptag Const			}
 mutable		{ ptag Mutable			}
 extern			{ ptag Extern			}
 ccall			{ ptag CCall			}

 infixr 		{ ptag InfixR			}
 infixl			{ ptag InfixL			}
 infix			{ ptag Infix			}

 data			{ ptag Data			}
 effect			{ ptag Effect			}
 region			{ ptag Region			}

 class			{ ptag Class			}
 instance		{ ptag Instance			}
 project		{ ptag Project			}

 let			{ ptag Let			}
 in			{ ptag In			}
 where			{ ptag Where			}

 case			{ ptag Case 			}
 of			{ ptag Of			}
 match			{ ptag Match			}

 if			{ ptag If			}
 then			{ ptag Then			}
 else			{ ptag Else			}

 do			{ ptag Do			}
 while			{ ptag While			}
 when			{ ptag When			}
 unless			{ ptag Unless			}
 break			{ ptag Break			}

 throw			{ ptag Throw			}
 try			{ ptag Try			}
 catch			{ ptag Catch			}
 with			{ ptag With			}

 forall			{ ptag Forall			}

 \:\:			{ ptag HasType			} 

 \<\:			{ ptag IsSubtypeOf		}
 \:\>			{ ptag IsSuptypeOf		}

 \:\$			{ ptag HasOpType		}
 \:\-			{ ptag HasConstraint		}

 \-\>			{ ptag RightArrow		}
 \(\)			{ ptag Unit			}
 \.\.			{ ptag DotDot			}

 \<\-			{ ptag LeftArrow		}
 \<\@\-			{ ptag LeftArrowLazy		}

 \| \-			{ ptag GuardCase		}
 \, \|			{ ptag GuardCaseC		}

 \| \#			{ ptag GuardUnboxed		}
 \, \#			{ ptag GuardUnboxedC		}

 \\ \=			{ ptag GuardDefault		}

 $sym $sym+		{ ptags (\s -> Symbol s)	}

 \#			{ ptag Hash			}
 \*			{ ptag Star			}
 \+			{ ptag Plus			}
 \%			{ ptag Percent			}
 \-			{ ptag Dash			}
 \@			{ ptag At			}
 \!			{ ptag Bang			}
 \/			{ ptag ForwardSlash		}
 \$			{ ptag Dollar			}
 \_			{ ptag Underscore		}
 \^			{ ptag Hat			}
 \~			{ ptag Tilde			}

 \<			{ ptag ABra			}
 \>			{ ptag AKet			}

 \{			{ ptag CBra			}
 \}			{ ptag CKet			}
 
 \(			{ ptag RBra			}
 \)			{ ptag RKet			}

 \[			{ ptag SBra			}
 \]			{ ptag SKet			}
 
 \\			{ ptag BackSlash		}
 \`			{ ptag BackTick			}
 \=			{ ptag Equals			}
 \,			{ ptag Comma			}
 \:			{ ptag Colon			}
 \;			{ ptag SemiColon		}
 \|			{ ptag Bar			}
 \.			{ ptag Dot			}
 \&			{ ptag And			}

 ($upper [$alpha $digit]* \.)* [\% \! \$]? $lower [$alpha $digit $varsym]*	 
 			{ ptags (\s -> Var   s) 	}

 ($upper [$alpha $digit]* \.)* [\% \! \$]? $upper [$alpha $digit $varsym]*	 
 			{ ptags (\s -> Con   s) 	}


 $sym+  $sym*		{ ptags (\s -> Symbol s)	}

 \" ($printable # \")* \" 
 			{ ptags (\s -> CString (read s))		}

 $digit+ \. $digit+	{ ptags (\s -> CFloat  $ read s)		}

 \-?$digit+		{ ptags (\s -> CInt    $ read s)	 	}
 \'\\n\'		{ ptags (\s -> CChar   $ read s)  		}
 \' . \'		{ ptags (\s -> CChar   $ read s)		}

 .			{ ptags (\s -> Junk s)				}


{ 

----------------------------------------------------------------------------------------------------

stage	= "Source.Lexer"


-- | Rag a generated token with its position
ptags :: (String -> Token) -> AlexPosn -> String -> TokenP
ptags 	  tokf (AlexPn _ l c)	s
 = TokenP 
 	{ token		= (tokf s)
	, tokenFile	= "unknown"
	, tokenLine 	= l
	, tokenColumn 	= c }

-- | Tag a token with its position
ptag ::  Token -> AlexPosn -> String -> TokenP
ptag	 tok (AlexPn _ l c) s
 = TokenP
 	{ token		= tok
	, tokenFile	= "unknown"
	, tokenLine	= l
	, tokenColumn	= c }


-- scan --------------------------------------------------------------------------------------------
-- This is the top level of the scanner
-- Add a newline to help ourselves out
scan 	:: String -> [TokenP]
scan ss	
	= (flip offside) []	-- apply the offside rule
	$ addStarts		-- add BlockStart / LineStart tokens in preparation for offside rule
	$ breakModules 		-- detect module names, and break into projections if required
	$ eatComments 		-- remove comments
	$ alexScanTokens ss

sourceTokens ts
	= catInt " " $ map showSourceP ts


lexOffside :: String -> String
lexOffside ss = sourceTokens $ scan ss

-- eatComments -------------------------------------------------------------------------------------
-- | Erase all the comments in this token stream
--	Also handles block comments.
eatComments ::	[TokenP] -> [TokenP]
eatComments 	[]	= []
eatComments	(t@TokenP { token = tok } : xs)
	| CommentLineStart	<- tok
	= eatComments $ dropWhile (\t -> not $ isToken t NewLine) xs

	| CommentBlockStart	<- tok
	= eatComments $ toTail $ dropWhile (\t -> not $ isToken t CommentBlockEnd) xs
	
	| otherwise
	= t : eatComments xs

toTail []	= []
toTail (x:xs)	= xs

-- breakModules ------------------------------------------------------------------------------------
-- | Break module qualifiers off var and con tokens
breakModules :: [TokenP] -> [TokenP]
breakModules toks
	= catMap breakModules' toks

breakModules' tok@(TokenP { token = tt })
	| Var str	<- tt
	, (mods, name)	<- breakModuleStr str
	= case mods of
	   [] 	-> [tok]
	   _	-> [ tok { token = ModuleName mods}
		   , tok { token = Dot }
		   , tok { token = Var name } ]
	
	| Con str	<- tt
	, (mods, name)	<- breakModuleStr str
	= case mods of 
	   [] 	-> [tok]
	   _	-> [ tok { token = ModuleName mods}
		   , tok { token = Dot }
		   , tok { token = Con name} ]
breakModules' t
	= [t]


-- Break module qualifiers off this variable name
breakModuleStr
	:: String		-- ^ variable name 
	-> ( [String]		-- ^ qualifiers
	   ,  String)		-- ^ base name

breakModuleStr str	
	| bits		<- breakOns '.' str

	-- at least one qualifier
 	, length bits > 1			

	-- lexer sanity 
	--	not more than one consecutive dot.
	, minimum (map length bits) > 0	

	-- peel of the front bits that look like module qualifiers
	, Just front	<- takeInit bits
	, Just back	<- takeLast bits
	, moduleBits	<- takeWhile (\b -> let Just h = takeHead b in isUpper h) front

	-- lexer sanity
	--	only one variable name
	--	ie not   Module.var1.var2
	, length bits == length moduleBits + 1
	= (moduleBits, back)
	
	| otherwise
	= ([], str)
	


-- addStarts ---------------------------------------------------------------------------------------
-- | Add block and line start tokens to this stream.
--	This is lifted straight from the Haskell98 report.
addStarts :: [TokenP] -> [TokenP]
addStarts ts
 = case forward ts of

	-- if the first lexeme of a module is not '{' or 'module' then start a new block
 	(t1 : tsRest)
	  |  not $ or $ map (isToken t1) [Module, CBra]
	  -> StartBlock (tokenColumn t1) : addStarts' (t1 : tsRest)
	
	  | otherwise
	  -> addStarts' (t1 : tsRest)
	  	
	-- empty file
	[]	-> []


addStarts'  :: [TokenP] -> [TokenP]
addStarts' []	= []
addStarts' (t1 : ts)

	-- We're starting a block
	| isBlockStart t1
	, []		<- forward ts
	= t1 : [StartBlock 0]
	
	| isBlockStart t1
	, t2 : tsRest	<- forward ts
	, not $ isToken t2 CBra
	= t1	: StartBlock (tokenColumn t2) 
		: addStarts' (t2 : tsRest)

	-- check for start of new line
	| isToken t1 NewLine
	, t2 : tsRest	<- forward ts
	, not $ isToken t2 CBra
	= StartLine (tokenColumn t2) : addStarts' (t2 : tsRest)

	-- eat up trailine newlines
	| isToken t1 NewLine
	= addStarts' ts

	-- a regular token
	| otherwise
	= t1 : addStarts' ts

isToken :: TokenP -> Token -> Bool
isToken TokenP { token = tok } t2	= tok == t2
isToken _ _				= False

-- check if a token is one that starts a block of statements.
isBlockStart :: TokenP -> Bool
isBlockStart TokenP { token = tok }
 = case tok of
 	Do		-> True
	Of		-> True
	Where		-> True
	Catch		-> True
	Match		-> True
	_		-> False

isBlockStart _
	= False

-- scan forward through the stream to get to the next non-newline token
forward :: [TokenP] 	-> [TokenP]
forward []		= []
forward (t1:ts)
	| TokenP { token = tok }	<- t1
	, NewLine			<- tok
	= forward ts
	
	| otherwise
	= t1 : ts



-- offside -----------------------------------------------------------------------------------------
-- | Apply the offside rule to this token stream.
--	They should have been processed with addStarts first to add the StartBlock/StartLine tokens.
--	This is lifted straight from the Haskell98 report.
--
type Context	= Int

offside	:: [TokenP] -> [Context] -> [TokenP]

offside ts ms
 = {- trace ( pprStr 
  	 $ "offside: \n"
 	 % "   ts = " % take 10 ts % "\n"
	 % "   ms = " % take 10 ms % "\n") $ -}
	offside' ts ms

-- avoid starting a new statement if the first token on the line is an '=', ',' or '->'
-- 	This allows the syntax of match and case expressions to line up nicely
-- eg
--	match 	| guard1
--		, guard2
--		= exp1
--
--		| guard2
--		= exp2
offside' (t1@(StartLine n) : t2 : ts) (m : ms)
	| isToken t2 Equals || isToken t2 Comma || isToken t2 RightArrow 
	= offside (t2 : ts) (m : ms)

-- line start
offside' (t@(StartLine n) : ts) (m : ms)

	-- add semicolon to get to the next statement in this block
	| m == n	= newSemiColon ts : offside ts (m : ms)

	-- end a block
	-- we keep the StartLine token in the recursion in case we're ending multiple blocks
	-- difference from Haskell98: add a semicolon as well
	
	| n < m		= newSemiColon ts : newCKet ts : offside (t : ts) ms

	-- indented continuation of this statement
	| otherwise
	= offside ts (m : ms)

-- block start
offside' (t@(StartBlock n) : ts) mm 
	
	-- enter into a nested context
	| m : ms	<- mm
	, n > m
	= newCBra ts : offside ts (n : m : ms)
	
	-- enter into an initial context (at top level)
	| []		<- mm
	, n > 0	
	= newCBra ts : offside ts (n : [])
	

	-- new context cannot be less indented than outer one
	-- This case should never happen. 
	--	There is no lexeme to start a new context at the end of the file
	| []		<- forward ts
	= panic stage
	$ "offside: Tried to open a new context at the end of the file."
	
	-- new context starts less than the current one
	| tNext : _	<- forward ts
	= dieWithUserError [ErrorLayoutLeft tNext]
			
	-- an empty block
	| otherwise
	= newCBra ts : newCKet ts : offside (StartLine n : ts) mm
	

-- pop contexts from explicit close braces
offside' (t@TokenP { token = CKet } : ts) mm

	-- make sure that explict open braces match explicit close braces
	| 0 : ms	<- mm
	= t : offside ts ms
	
	-- nup
	| tNext : _	<- forward ts
	= dieWithUserError [ErrorLayoutNoBraceMatch tNext]

-- push contexts for explicit open braces
offside' (t@TokenP { token = CBra } : ts) ms
	
	= t : offside ts (0 : ms)

-- i'm totally ignoring the rule that inserts
--	close braces on parse errors -- that's crazy talk.

offside' (t : ts) ms	= t : offside ts ms

offside' [] []		= []

-- close off remaining contexts once we've reached the end of the stream.
offside' [] (m : ms)	= newCKet [] : offside [] ms

	
-- When generating new source tokens, take the position from the first non-newline token in this list
newCBra :: [TokenP] -> TokenP
newCBra ts	= (takeTok ts) { token = CBra }

newCKet :: [TokenP] -> TokenP
newCKet	ts	= (takeTok ts) { token = CKet } 

newSemiColon :: [TokenP] -> TokenP
newSemiColon ts = (takeTok ts) 	{ token = SemiColon }

takeTok :: [TokenP] -> TokenP
takeTok tt 
 = case forward tt of
 	[]	-> TokenP { token = Junk "", tokenFile = [], tokenLine = 0, tokenColumn = 0}
	(t:_)	-> t



}




