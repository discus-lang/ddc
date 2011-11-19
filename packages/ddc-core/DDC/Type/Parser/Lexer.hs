
-- | Reference lexer for the type parser.
module DDC.Type.Parser.Lexer
        ( isTyConName, isTyConStart, isTyConBody
        , isTyVarName, isTyVarStart, isTyVarBody
        , Tok(..)
        , lexType)
where
import Data.Char


-- TyCon names ------------------------------------------------------------------------------------
-- | String is a type constructor name.
isTyConName :: String -> Bool
isTyConName []          = False
isTyConName (c:cs)      = isTyConStart c && and (map isTyConBody cs)


-- | Character can start a type constructor name.
isTyConStart :: Char -> Bool
isTyConStart = isUpper


-- | Charater can be part of a type constructor body.
isTyConBody  :: Char -> Bool
isTyConBody c           = isUpper c || isLower c || isDigit c || c == '_'
        

-- TyVar names ------------------------------------------------------------------------------------
-- | String is a type variable name.
isTyVarName :: String -> Bool
isTyVarName []          = False
isTyVarName (c:cs)      = isTyVarStart c && and (map isTyVarBody cs)


-- | Charater can start a type variable name.
isTyVarStart :: Char -> Bool
isTyVarStart = isLower
        

-- | Character can be part of a type variable body.
isTyVarBody  :: Char -> Bool
isTyVarBody c
        = isUpper c || isLower c || isDigit c || c == '_' || c == '\''


---------------------------------------------------------------------------------------------------
-- | Tokens returned by the type lexer.
data Tok
        = Tok      String
        | TokError String
        deriving (Eq, Show)


-- | Lex a string into type tokens.
lexType :: String -> [Tok]
lexType str
 = concatMap lexWord $ words str
 where 
  lexWord w
   = case w of
        []              -> []        

        -- Function Constructors
        '~' : '>' : w'  -> Tok "~>" : lexWord w'
        '-' : '>' : w'  -> Tok "->" : lexWord w'
        '-' : '(' : w'  -> Tok "-(" : lexWord w'
        ')' : '>' : w'  -> Tok ")>" : lexWord w'

        -- Brackets
        '(' : w'        -> Tok "("  : lexWord w'
        ')' : w'        -> Tok ")"  : lexWord w'
        '[' : w'        -> Tok "["  : lexWord w'
        ']' : w'        -> Tok "]"  : lexWord w'
        ':' : w'        -> Tok "]"  : lexWord w'
        '.' : w'        -> Tok "]"  : lexWord w'
        
        -- Bottoms
        '!' : '0' : w'  -> Tok "!0" : lexWord w'
        '$' : '0' : w'  -> Tok "!0" : lexWord w'

        -- Sort Constructors
        '*' : '*' : w'  -> Tok "**" : lexWord w'
        '@' : '@' : w'  -> Tok "@@" : lexWord w'        

        -- Kind Constructors
        '*' : w'        -> Tok "*"  : lexType w'
        '%' : w'        -> Tok "%"  : lexType w'
        '!' : w'        -> Tok "!"  : lexType w'
        '$' : w'        -> Tok "$"  : lexType w'
        '@' : w'        -> Tok "@"  : lexType w'

        -- Type Constructor
        c : cs
         | isTyConStart c
         , (body, rest)        <- span isTyConBody cs
         -> Tok (c:body) : lexType rest
        
        -- Type Variable
        c : cs
         | isTyVarStart c
         , (body, rest)         <- span isTyVarBody cs
         -> Tok (c:body) : lexType rest
        
        -- Error
        _               -> [TokError w]


