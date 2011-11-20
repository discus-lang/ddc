
-- | Reference lexer for the type parser. Slow but Simple.
module DDC.Type.Parser.Lexer
        ( isConName, isConStart, isConBody
        , isVarName, isVarStart, isVarBody
        , lexType)
where
import Data.Char


-- TyCon names ------------------------------------------------------------------------------------
-- | String is a constructor name.
isConName :: String -> Bool
isConName []          = False
isConName (c:cs)      = isConStart c && and (map isConBody cs)


-- | Character can start a constructor name.
isConStart :: Char -> Bool
isConStart = isUpper


-- | Charater can be part of a constructor body.
isConBody  :: Char -> Bool
isConBody c           = isUpper c || isLower c || isDigit c || c == '_'
        

-- TyVar names ------------------------------------------------------------------------------------
-- | String is a variable name.
isVarName :: String -> Bool
isVarName []          = False
isVarName (c:cs)      = isVarStart c && and (map isVarBody cs)


-- | Charater can start a variable name.
isVarStart :: Char -> Bool
isVarStart = isLower
        

-- | Character can be part of a variable body.
isVarBody  :: Char -> Bool
isVarBody c
        = isUpper c || isLower c || isDigit c || c == '_' || c == '\''


---------------------------------------------------------------------------------------------------
-- | Lex a string into type tokens.
--   If there are any `Nothing` elements in the returned list then there was a lexical error.
lexType :: String -> [Maybe String]
lexType str
 = concatMap lexWord $ words str
 where 
  lexWord w
   = case w of
        []              -> []        

        -- Function Constructors
        '~' : '>' : w'  -> Just "~>" : lexWord w'
        '-' : '>' : w'  -> Just "->" : lexWord w'
        '-' : '(' : w'  -> Just "-(" : lexWord w'
        ')' : '>' : w'  -> Just ")>" : lexWord w'

        -- Brackets
        '(' : w'        -> Just "("  : lexWord w'
        ')' : w'        -> Just ")"  : lexWord w'
        '[' : w'        -> Just "["  : lexWord w'
        ']' : w'        -> Just "]"  : lexWord w'
        ':' : w'        -> Just ":"  : lexWord w'
        '.' : w'        -> Just "."  : lexWord w'
        ',' : w'        -> Just ","  : lexWord w'
        
        -- Bottoms
        '*' : '0' : w'  -> Just "*0" : lexWord w'
        '%' : '0' : w'  -> Just "%0" : lexWord w'
        '!' : '0' : w'  -> Just "!0" : lexWord w'
        '$' : '0' : w'  -> Just "$0" : lexWord w'

        -- Sort Constructors
        '*' : '*' : w'  -> Just "**" : lexWord w'
        '@' : '@' : w'  -> Just "@@" : lexWord w'        

        -- Kind Constructors
        '*' : w'        -> Just "*"  : lexType w'
        '%' : w'        -> Just "%"  : lexType w'
        '!' : w'        -> Just "!"  : lexType w'
        '$' : w'        -> Just "$"  : lexType w'
        '@' : w'        -> Just "@"  : lexType w'
        
        -- Symbolic Type Constructors
        '+' : w'        -> Just "+"  : lexType w'

        -- Named Type Constructors
        c : cs
         | isConStart c
         , (body, rest)        <- span isConBody cs
         -> Just (c:body) : lexType rest
        
        -- Type Variable
        c : cs
         | isVarStart c
         , (body, rest)         <- span isVarBody cs
         -> Just (c:body) : lexType rest
        
        -- Error
        _               -> [Nothing]


