
module Scanner.Named
        (scanNamed)
where
import Token
import Text.Lexer.Inchworm.Char
import qualified Data.Char      as Char


-- Named  ---------------------------------------------------------------------
-- | Scan a named thing.
scanNamed  :: Scanner IO Location [Char] (Location, Token)
scanNamed = alt scanKeyVar scanCon


-- Variables and Keywords -----------------------------------------------------
keywords
 =      [ "import",     "export" 
        , "box",        "run"
        , "let",        "in"
        , "case",       "of"
        , "do"
        , "if",         "then",         "else"]


-- | Scan a keyword or variable.
scanKeyVar :: Scanner IO Location [Char] (Location, Token)
scanKeyVar
 = munchPred Nothing matchKeyVar acceptKeyVar
 where
        matchKeyVar  :: Int -> Char -> Bool
        matchKeyVar 0 c    = isVarStart c
        matchKeyVar _ c    = isVarBody  c

        acceptKeyVar :: [Char] -> Maybe Token
        acceptKeyVar cs
                | elem cs keywords      = Just $ KKeyWord cs
                | otherwise             = Just $ KVar     cs


isVarStart :: Char -> Bool
isVarStart c
 =  Char.isLower c
 || c == '?'

isVarBody  :: Char -> Bool
isVarBody c
 =  Char.isAlpha c
 || Char.isDigit c
 || c == '_' || c == '\'' || c == '$' || c == '#'


-- Constructors ---------------------------------------------------------------
scanCon   :: Scanner IO Location [Char] (Location, Token)
scanCon
 = munchPred Nothing matchCon acceptCon
 where  
        matchCon 0 c    = isConStart c
        matchCon _ c    = isConBody  c

        acceptCon cs    = Just $ KCon cs


-- | Character can start a constructor name
isConStart :: Char -> Bool
isConStart c
 = Char.isUpper c


-- | Character can be in the body of a constructor name.
isConBody  :: Char -> Bool
isConBody c
 =  Char.isAlpha c
 || Char.isDigit c 
 || c == '_' || c == '\'' || c == '#'

