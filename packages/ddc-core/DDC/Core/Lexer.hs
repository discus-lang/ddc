
-- | Reference lexer for core langauge parser. Slow but Simple.
--
--   The lexers here all use 'String' in place of a real name type.
--   After applying these functions to the program text, we need
--   to use `renameTok` tok convert the strings in `TokNamed` tokens
--   into the name type specific to the langauge fragment to be parsed.
--
module DDC.Core.Lexer
        ( module DDC.Core.Lexer.Tokens
        , module DDC.Core.Lexer.Names

          -- * Lexer
        , lexModuleWithOffside
        , lexExp)
where
import DDC.Core.Lexer.Offside
import DDC.Core.Lexer.Comments
import DDC.Core.Lexer.Names
import DDC.Core.Lexer.Tokens
import DDC.Data.SourcePos
import DDC.Data.Token
import Data.Char
import Data.List


-- Module ---------------------------------------------------------------------
-- | Lex a module and apply the offside rule.
--
--   Automatically drop comments from the token stream along the way.
--
lexModuleWithOffside 
        :: FilePath     -- ^ Path to source file, for error messages.
        -> Int          -- ^ Starting line number.
        -> String       -- ^ String containing program text.
        -> [Token (Tok String)]

lexModuleWithOffside sourceName lineStart str
 = {-# SCC lexWithOffside #-}
        applyOffside [] 
        $ addStarts
        $ dropComments 
        $ lexString sourceName lineStart str


-- Exp ------------------------------------------------------------------------
-- | Lex a string into tokens.
--
--   Automatically drop comments from the token stream along the way.
--
lexExp  :: FilePath     -- ^ Path to source file, for error messages.
        -> Int          -- ^ Starting line number.
        -> String       -- ^ String containing program text.
        -> [Token (Tok String)]

lexExp sourceName lineStart str
 = {-# SCC lexExp #-}
        dropNewLines
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

        -- Whitespace
        ' '  : w'        -> lexMore 1 w'
        '\t' : w'        -> lexMore 8 w'

        -- Literal values
        -- This needs to come before the rule for '-'
        c : cs
         | isDigit c
         , (body, rest)         <- span isLitBody cs
         -> tokN (KLit (c:body))                 : lexMore (length (c:body)) rest

        '-' : c : cs
         | isDigit c
         , (body, rest)         <- span isLitBody cs
         -> tokN (KLit ('-':c:body))                 : lexMore (length (c:body)) rest

        -- Meta tokens
        '{'  : '-' : w'  -> tokM KCommentBlockStart : lexMore 2 w'
        '-'  : '}' : w'  -> tokM KCommentBlockEnd   : lexMore 2 w'
        '-'  : '-' : w'  -> tokM KCommentLineStart  : lexMore 2 w'
        '\n' : w'        -> tokM KNewLine           : lexWord (line + 1) 1 w'


        -- The unit data constructor
        '(' : ')' : w'   -> tokA KDaConUnit      : lexMore 2 w'

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
        name
         | Just w'      <- stripPrefix "Pure"    name -> tokA KBotEffect   : lexMore 2 w'
         | Just w'      <- stripPrefix "Empty"   name -> tokA KBotClosure  : lexMore 2 w'

        -- Baked in Sort Constructors
        name 
         | Just w'      <- stripPrefix "Comp"    name -> tokA KSortComp    : lexMore 4 w'
         | Just w'      <- stripPrefix "Prop"    name -> tokA KSortProp    : lexMore 4 w'

        -- Baked in Kind Constructors
        name
         | Just w'      <- stripPrefix "Data"    name -> tokA KKindValue   : lexMore 4 w'
         | Just w'      <- stripPrefix "Region"  name -> tokA KKindRegion  : lexMore 6 w'
         | Just w'      <- stripPrefix "Effect"  name -> tokA KKindEffect  : lexMore 6 w'
         | Just w'      <- stripPrefix "Closure" name -> tokA KKindClosure : lexMore 7 w'
         | Just w'      <- stripPrefix "Witness" name -> tokA KKindWitness : lexMore 7 w'

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

