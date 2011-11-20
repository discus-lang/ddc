
-- | Reference lexer for core langauge parser. Slow but Simple.
module DDC.Core.Parser.Lexer
        ( isConName, isConStart, isConBody
        , isVarName, isVarStart, isVarBody
        , lexExp)
where
import DDC.Type.Parser.Lexer
import Data.List

---------------------------------------------------------------------------------------------------
-- | Lex a string into type tokens.
--   
--   * This is a conservative extension of the core type parser.
--   * If there are any `Nothing` elements in the returned list then there was a lexical error.
lexExp :: String -> [Maybe String]
lexExp str
 = concatMap lexWord $ words str
 where 
  lexWord w
   = case w of
        []              -> []        

        -- Function Constructors
        '~' : '>' : w'  -> Just "~>" : lexExp w'
        '-' : '>' : w'  -> Just "->" : lexExp w'
        '-' : '(' : w'  -> Just "-(" : lexExp w'
        ')' : '>' : w'  -> Just ")>" : lexExp w'

        -- Brackets
        '('  : w'       -> Just "("  : lexExp w'
        ')'  : w'       -> Just ")"  : lexExp w'
        '['  : w'       -> Just "["  : lexExp w'
        ']'  : w'       -> Just "]"  : lexExp w'
        '{'  : w'       -> Just "{"  : lexExp w'
        '}'  : w'       -> Just "}"  : lexExp w'
        ':'  : w'       -> Just ":"  : lexExp w'
        '.'  : w'       -> Just "."  : lexExp w'
        ','  : w'       -> Just ","  : lexExp w'
        '\\' : w'       -> Just "\\" : lexExp w'
        ';'  : w'       -> Just ";"  : lexExp w'
        
        -- Bottoms
        '*' : '0' : w'  -> Just "*0" : lexExp w'
        '%' : '0' : w'  -> Just "%0" : lexExp w'
        '!' : '0' : w'  -> Just "!0" : lexExp w'
        '$' : '0' : w'  -> Just "$0" : lexExp w'

        -- Sort Constructors
        '*' : '*' : w'  -> Just "**" : lexExp w'
        '@' : '@' : w'  -> Just "@@" : lexExp w'        

        -- Kind Constructors
        '*' : w'        -> Just "*"  : lexExp w'
        '%' : w'        -> Just "%"  : lexExp w'
        '!' : w'        -> Just "!"  : lexExp w'
        '$' : w'        -> Just "$"  : lexExp w'
        '@' : w'        -> Just "@"  : lexExp w'
        
        -- Symbolic Type Constructors
        '+' : w'        -> Just "+"  : lexExp w'

        -- Named Constructors
        c : cs
         | isConStart c
         , (body, rest) <- span isConBody cs
         -> Just (c:body) : lexExp rest
                
        -- Type Variable
        c : cs
         | isVarStart c
         , (body, rest) <- span isVarBody cs
         -> Just (c:body) : lexExp rest

        -- Keywords
        _
         | Just key     <- find (\key -> isPrefixOf key w) keywords
         -> Just key : lexExp (drop (length key) w)
         
        -- Error
        _               -> [Nothing]
        

-- | Textual keywords in the core language.
keywords :: [String]
keywords
 =      [ "letrec", "let", "local", "in"
        , "purify", "emptify"
        , "case",   "of" ]
