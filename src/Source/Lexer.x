
{ 
module Source.Lexer
(
	scan
)

where

import Source.Token

}

%wrapper "posn"

$digit	= 0-9

$upper	= [A-Z]
$lower  = [a-z]
$alpha	= [$lower $upper]

$varsym	= [\' \_]
$var    = [$alpha $digit $varsym]

-- $ssym	= [\\ \= \: \|]
$sym	= [\! \# \$ \% \& \* \+ \/ \< \> \? \@ \^ \- \~ \\ \= \: \|]


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
 \:\*			{ ptag HasTypeQuant		}

 \<\:			{ ptag IsSubtypeOf		}
 \<\*			{ ptag IsSubtypeOfQuant		}

 \:\:\:			{ ptag HasTypeExact		}
 \:\:\*			{ ptag HasTypeExactQuant	}

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
 \/			{ ptag FSlash			}
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
 
 \\			{ ptag BSlash			}
 \`			{ ptag BTick			}
 \=			{ ptag Equals			}
 \,			{ ptag Comma			}
 \:			{ ptag Colon			}
 \;			{ ptag SColon			}
 \|			{ ptag Bar			}
 \.			{ ptag Dot			}

 $upper $var*		{ ptags (\s -> Tycon s)		}
 $lower $var*		{ ptags (\s -> Var   s) 	}
 $sym+  $sym*		{ ptags (\s -> Symbol s)	}

 \" ($printable # \")* \" 
 			{ ptags (\s -> CString (read s))		}

 $digit+ \. $digit+	{ ptags (\s -> CFloat  $ read s)		}

 $digit+		{ ptags (\s -> CInt    $ read s)	 	}
 \'\\n\'		{ ptags (\s -> CChar   $ read s)  		}
 \' . \'		{ ptags (\s -> CChar   $ read s)		}

 .			{ ptags (\s -> Junk s)				}

{ 

ptags :: (String -> Token) ->	AlexPosn -> 	String -> TokenP
ptags 	  tokf			(AlexPn _ l c)	s
 = TokenP 
 	{ token		= (tokf s)
	, file		= "unknown"
	, line 		= l
	, column 	= c - 1 }

ptag ::  Token ->		AlexPosn -> 	String -> TokenP
ptag	 tok			(AlexPn _ l c)	s
 = TokenP
 	{ token		= tok
	, file		= "unknown"
	, line		= l
	, column	= c - 1 }


eatComments ::	[TokenP] -> [TokenP]
eatComments 	[]	= []
eatComments	(tokenp:xs)
 = case token tokenp of
 	CommentLineStart	-> eatComments $ tail $ dropWhile (\t -> token t /= NewLine) xs
	CommentBlockStart	-> eatComments $ tail $ dropWhile (\t -> token t /= CommentBlockEnd) xs
	NewLine			-> eatComments xs	
	_			-> tokenp : eatComments xs


-- The scanner seems to generate a "tail: empty list" error if
-- the file does not finish with a newline. (Might be the token printer?)
--	Add one here to fix this, shouldn't hurt anything.
--
scan 		:: String -> [TokenP]
scan ss		=  eatComments $ alexScanTokens (ss ++ "\n")

-- \=\=			{ ptag (Symbol "==")		}
-- \|\|			{ ptag (Symbol "||")		}

}

