
module DDC.Core.Lexer.Token.Names
        ( -- * Variable names
          scanVarName
        , acceptVarName
        , isVarName
        , isVarStart
        , isVarBody

          -- * Constructor names
        , scanConName
        , acceptConName
        , isConName
        , isConStart
        , isConBody)
where
import Text.Lexer.Inchworm.Char
import DDC.Data.ListUtils
import qualified Data.Char      as Char


-- Variable names ---------------------------------------------------------------------------------
-- | Scanner for variable anmes.
scanVarName   :: Scanner IO Location [Char] (Location, String)
scanVarName 
 = munchPred Nothing matchVarName acceptVarName


-- | Match a variable name.
matchVarName :: Int -> Char -> Bool
matchVarName 0 c        = isVarStart c
matchVarName _ c        = isVarBody  c


-- | Accept a variable name.
acceptVarName :: String -> Maybe String
acceptVarName ss
        | isVarName ss  = Just ss
        | otherwise     = Nothing


-- | Check if this string is a variable name.
isVarName :: String -> Bool
isVarName str
 = case str of
     []          -> False
     c : cs 
        |  isVarStart c 
        ,  and (map isVarBody cs)
        -> True
        
        | _ : _         <- cs
        ,  Just initCs   <- takeInit cs
        ,  isVarStart c
        ,  and (map isVarBody initCs)
        ,  last cs == '#'
        -> True

        | otherwise
        -> False


-- | Charater can start a variable name.
isVarStart :: Char -> Bool
isVarStart c
        =  Char.isLower c
        || c == '?'
        || c == '_'
        

-- | Character can be part of a variable body.
isVarBody  :: Char -> Bool
isVarBody c
        =  Char.isUpper c 
        || Char.isLower c 
        || Char.isDigit c 
        || c == '_' 
        || c == '\'' 
        || c == '$'
        || c == '#'


-- Constructor names ------------------------------------------------------------------------------
-- | Scanner for constructor names.
scanConName   :: Scanner IO Location [Char] (Location, String)
scanConName 
 = munchPred Nothing matchConName acceptConName


-- | Match a constructor name.
matchConName  :: Int -> Char -> Bool
matchConName 0 c        = isConStart c
matchConName _ c        = isConBody  c


-- | Accept a constructor name.
acceptConName :: String -> Maybe String
acceptConName ss
        | isConName ss  = Just ss
        | otherwise     = Nothing


-- | String is a constructor name.
isConName :: String -> Bool
isConName str
 = case str of
     []          -> False
     c : cs 
        |  isConStart c 
        ,  and (map isConBody cs)
        -> True
        
        | _ : _         <- cs
        ,  Just initCs   <- takeInit cs
        ,  isConStart c
        ,  and (map isConBody initCs)
        ,  last cs == '#'
        -> True

        | otherwise
        -> False

-- | Character can start a constructor name.
isConStart :: Char -> Bool
isConStart 
        = Char.isUpper


-- | Charater can be part of a constructor body.
isConBody  :: Char -> Bool
isConBody c     
        =  Char.isUpper c 
        || Char.isLower c 
        || Char.isDigit c 
        || c == '_'
        || c == '\''
        || c == '#'
        
