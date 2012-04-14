
-- | Reference lexer for core langauge parser. Slow but Simple.
module DDC.Core.Lexer
        ( module DDC.Core.Lexer.Tokens
        , module DDC.Core.Lexer.Names

          -- * Lexer
        , lexModuleWithOffside
        , lexExp)
where
import DDC.Base.Lexer
import DDC.Core.Lexer.Offside
import DDC.Core.Lexer.Comments
import DDC.Core.Lexer.Names
import DDC.Core.Lexer.Tokens
import Data.Char


-- Module ---------------------------------------------------------------------
-- | Lex a module and apply the offside rule.
lexModuleWithOffside :: String -> Int -> String -> [Token (Tok String)]
lexModuleWithOffside sourceName lineStart str
        = applyOffside []
        $ addStarts
        $ dropComments 
        $ lexString sourceName lineStart str


-- Exp ------------------------------------------------------------------------
-- | Lex a string into tokens.
--
--   Automatically drop comments from the token stream along the way.
--
lexExp :: String -> Int -> String -> [Token (Tok String)]
lexExp sourceName lineStart str
        = dropNewLines
        $ dropComments
        $ lexString sourceName lineStart str


-- Generic --------------------------------------------------------------------
lexString :: String -> Int -> String -> [Token (Tok String)]
lexString sourceName lineStart str
        = lexWord lineStart 1 str
 where 
  lexWord :: Int -> Int -> String -> [Token (Tok String)]
  lexWord line column w
   = let  tok t = Token t (SourcePos sourceName line column)
          tokM  = tok . KM
          tokA  = tok . KA
          tokN  = tok . KN

          lexMore n rest
           = lexWord line (column + n) rest

     in case w of
        []               -> []        

        ' '  : w'        -> lexMore 1 w'
        '\t' : w'        -> lexMore 8 w'

        -- Meta tokens
        '-'  : '-' : w'  -> tokM KCommentLineStart : lexMore 2 w'
        '\n' : w'        -> tokM KNewLine          : lexWord (line + 1) 1 w'


        -- The unit data constructor
        '(' : ')' : w'   -> tokN (KCon "()")     : lexMore 2 w'

        -- Compound Parens
        '['  : ':' : w'  -> tokA KSquareColonBra : lexMore 2 w'
        ':'  : ']' : w'  -> tokA KSquareColonKet : lexMore 2 w'
        '<'  : ':' : w'  -> tokA KAngleColonBra  : lexMore 2 w'
        ':'  : '>' : w'  -> tokA KAngleColonKet  : lexMore 2 w'

        -- Function Constructors
        '~'  : '>'  : w' -> tokA KArrowTilde     : lexMore 2 w'
        '-'  : '>'  : w' -> tokA KArrowDash      : lexMore 2 w'
        '<'  : '-'  : w' -> tokA KArrowDashLeft  : lexMore 2 w'
        '='  : '>'  : w' -> tokA KArrowEquals    : lexMore 2 w'

        -- Compound symbols
        ':'  : ':'  : w' -> tokA KColonColon     : lexMore 2 w'
        '/'  : '\\' : w' -> tokA KBigLambda      : lexMore 2 w'

        -- Debruijn indices
        '^'  : cs
         |  (ds, rest)   <- span isDigit cs
         ,  length ds >= 1
         -> tokA (KIndex (read ds))              : lexMore (1 + length ds) rest         

        -- Parens
        '('  : w'       -> tokA KRoundBra        : lexMore 1 w'
        ')'  : w'       -> tokA KRoundKet        : lexMore 1 w'
        '['  : w'       -> tokA KSquareBra       : lexMore 1 w'
        ']'  : w'       -> tokA KSquareKet       : lexMore 1 w'
        '{'  : w'       -> tokA KBraceBra        : lexMore 1 w'
        '}'  : w'       -> tokA KBraceKet        : lexMore 1 w'
        '<'  : w'       -> tokA KAngleBra        : lexMore 1 w'
        '>'  : w'       -> tokA KAngleKet        : lexMore 1 w'            

        -- Punctuation
        '.'  : w'       -> tokA KDot             : lexMore 1 w'
        '|'  : w'       -> tokA KBar             : lexMore 1 w'
        '^'  : w'       -> tokA KHat             : lexMore 1 w'
        '+'  : w'       -> tokA KPlus            : lexMore 1 w'
        ':'  : w'       -> tokA KColon           : lexMore 1 w'
        ','  : w'       -> tokA KComma           : lexMore 1 w'
        '\\' : w'       -> tokA KBackSlash       : lexMore 1 w'
        ';'  : w'       -> tokA KSemiColon       : lexMore 1 w'
        '_'  : w'       -> tokA KUnderscore      : lexMore 1 w'
        '='  : w'       -> tokA KEquals          : lexMore 1 w'
        '&'  : w'       -> tokA KAmpersand       : lexMore 1 w'
        '-'  : w'       -> tokA KDash            : lexMore 1 w'
        
        -- Bottoms
        '!' : '0' : w'  -> tokA KBotEffect       : lexMore 2 w'
        '$' : '0' : w'  -> tokA KBotClosure      : lexMore 2 w'

        -- Sort Constructors
        '*' : '*' : w'  -> tokA KSortComp        : lexMore 2 w'
        '@' : '@' : w'  -> tokA KSortProp        : lexMore 2 w'        

        -- Kind Constructors
        '*' : w'        -> tokA KKindValue       : lexMore 1 w'
        '%' : w'        -> tokA KKindRegion      : lexMore 1 w'
        '!' : w'        -> tokA KKindEffect      : lexMore 1 w'
        '$' : w'        -> tokA KKindClosure     : lexMore 1 w'
        '@' : w'        -> tokA KKindWitness     : lexMore 1 w'
        
        -- Literal values
        c : cs
         | isDigit c
         , (body, rest)         <- span isLiteralish cs
         -> tokN (KLit (c:body))                 : lexMore (length (c:body)) rest
        
        -- Named Constructors
        c : cs
         | isConStart c
         , (body,  rest)        <- span isConBody cs
         , (body', rest')       <- case rest of
                                        '#' : rest'     -> (body ++ "#", rest')
                                        _               -> (body, rest)
         -> let readNamedCon s
                 | Just twcon   <- readTwConBuiltin s
                 = tokA (KTwConBuiltin twcon)    : lexMore (length s) rest'
                 
                 | Just tccon   <- readTcConBuiltin s
                 = tokA (KTcConBuiltin tccon)    : lexMore (length s) rest'
                 
                 | Just con     <- readCon s
                 = tokN (KCon con)               : lexMore (length s) rest'
               
                 | otherwise    
                 = [tok (KJunk [c])]
                 
            in  readNamedCon (c : body')

        -- Keywords, Named Variables and Witness constructors
        c : cs
         | isVarStart c
         , (body,  rest)        <- span isVarBody cs
         , (body', rest')       <- case rest of
                                        '#' : rest'     -> (body ++ "#", rest')
                                        _               -> (body, rest)
         -> let readNamedVar s
                 | Just t <- lookup s keywords
                 = tok t                   : lexMore (length s) rest'

                 | Just wc      <- readWbConBuiltin s
                 = tokA (KWbConBuiltin wc) : lexMore (length s) rest'
         
                 | Just v       <- readVar s
                 = tokN (KVar v)           : lexMore (length s) rest'

                 | otherwise
                 = [tok (KJunk [c])]

            in  readNamedVar (c : body')

        -- Some unrecognised character.
        -- We still need to keep lexing as this may be in a comment.
        c : cs   -> (tok $ KJunk [c]) : lexMore 1 cs


isLiteralish :: Char -> Bool
isLiteralish c
        =  isDigit c
        || c == 'b' || c == 'o' || c == 'x'
        || c == 'w' || c == 'i' 
        || c == '#'
