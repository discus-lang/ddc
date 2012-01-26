
-- | Reference lexer for the type parser. Simple but Slow.
module DDC.Type.Parser.Lexer
        ( -- * Constructors
          isConName, isConStart, isConBody
        , readTwConBuiltin
        , readTcConBuiltin
        , readCon
        
          -- * Variables
        , isVarName, isVarStart, isVarBody
        , readVar)
where
import DDC.Type.Exp
import Data.Char


-- TyCon names ------------------------------------------------------------------------------------
-- | String is a constructor name.
isConName :: String -> Bool
isConName str
 = case str of
     []          -> False
     (c:cs)      
        | isConStart c 
        , and (map isConBody cs)
        -> True
        
        | _ : _         <- cs
        , isConStart c
        , and (map isConBody (init cs))
        , last cs == '#'
        -> True

        | otherwise
        -> False

-- | Character can start a constructor name.
isConStart :: Char -> Bool
isConStart = isUpper


-- | Charater can be part of a constructor body.
isConBody  :: Char -> Bool
isConBody c           = isUpper c || isLower c || isDigit c || c == '_'
        

-- | Read a named `TwCon`. 
readTwConBuiltin :: String -> Maybe TwCon
readTwConBuiltin ss
 = case ss of
        "Global"        -> Just TwConGlobal
        "DeepGlobal"    -> Just TwConDeepGlobal
        "Const"         -> Just TwConConst
        "DeepConst"     -> Just TwConDeepConst
        "Mutable"       -> Just TwConMutable
        "DeepMutable"   -> Just TwConDeepMutable
        "Lazy"          -> Just TwConLazy
        "HeadLazy"      -> Just TwConHeadLazy
        "Manifest"      -> Just TwConManifest
        "Pure"          -> Just TwConPure
        "Empty"         -> Just TwConEmpty
        _               -> Nothing


-- | Read a builtin `TcCon` with a non-symbolic name, 
--   ie not '->'.
readTcConBuiltin :: String -> Maybe TcCon
readTcConBuiltin ss
 = case ss of
        "Read"          -> Just TcConRead
        "HeadRead"      -> Just TcConHeadRead
        "DeepRead"      -> Just TcConDeepRead
        "Write"         -> Just TcConWrite
        "DeepWrite"     -> Just TcConDeepWrite
        "Alloc"         -> Just TcConAlloc
        "DeepAlloc"     -> Just TcConDeepAlloc
        "Use"           -> Just TcConUse
        "DeepUse"       -> Just TcConDeepUse
        _               -> Nothing


-- | Read a named, user defined `TcCon`.
--
--   We won't know its kind, so fill this in with the Bottom element for 
--   computatation kinds (**0).
readCon :: String -> Maybe String
readCon ss
        | isConName ss  = Just ss
        | otherwise     = Nothing


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


-- | Read a named, user defined variable.
readVar :: String -> Maybe String
readVar ss
        | isVarName ss  = Just ss
        | otherwise     = Nothing

