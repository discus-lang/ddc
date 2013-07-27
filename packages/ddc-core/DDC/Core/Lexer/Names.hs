
module DDC.Core.Lexer.Names
        ( -- * Keywords
          keywords

          -- * Builtin constructors
        , readSoConBuiltin
        , readKiConBuiltin
        , readTwConBuiltin
        , readTcConBuiltin
        , readWbConBuiltin

          -- * Variable names
        , isVarName
        , isVarStart
        , isVarBody
        , readVar

          -- * Constructor names
        , isConName
        , isConStart
        , isConBody
        , readCon

          -- * Operator names
        , isOpName
        , isOpStart
        , isOpBody

          -- * Literal names
        , isLitName
        , isLitStart
        , isLitBody)
where
import DDC.Core.Exp
import DDC.Core.Lexer.Tokens
import DDC.Data.ListUtils
import Data.Char
import Data.List


-- | Textual keywords in the core language.
keywords :: [(String, Tok n)]
keywords
 =      [ ("module",     KA KModule)
        , ("imports",    KA KImports)
        , ("exports",    KA KExports)
        , ("in",         KA KIn)
        , ("of",         KA KOf) 
        , ("letrec",     KA KLetRec)
        , ("letregions", KA KLetRegions)
        , ("letregion",  KA KLetRegion)
        , ("withregion", KA KWithRegion)
        , ("let",        KA KLet)
        , ("lazy",       KA KLazy)
        , ("case",       KA KCase)
        , ("purify",     KA KPurify)
        , ("forget",     KA KForget)
        , ("suspend",    KA KSuspend)
        , ("run",        KA KRun)
        , ("type",       KA KType)
        , ("weakeff",    KA KWeakEff)
        , ("weakclo",    KA KWeakClo)
        , ("with",       KA KWith)
        , ("where",      KA KWhere) 
        , ("do",         KA KDo)
        , ("match",      KA KMatch)
        , ("else",       KA KElse) ]


-- | Read a named sort constructor.
readSoConBuiltin :: String -> Maybe SoCon
readSoConBuiltin ss
 = case ss of
        "Prop"          -> Just SoConProp
        "Comp"          -> Just SoConComp
        _               -> Nothing


-- | Read a named kind constructor.
readKiConBuiltin :: String -> Maybe KiCon
readKiConBuiltin ss
 = case ss of
        "Witness"       -> Just KiConWitness
        "Data"          -> Just KiConData
        "Region"        -> Just KiConRegion
        "Effect"        -> Just KiConEffect
        "Closure"       -> Just KiConClosure
        _               -> Nothing


-- | Read a named witness type constructor.
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
        "Purify"        -> Just TwConPure
        "Emptify"       -> Just TwConEmpty
        "Disjoint"      -> Just TwConDisjoint
        "Distinct"      -> Just (TwConDistinct 2)
        _               -> readTwConWithArity ss


readTwConWithArity :: String -> Maybe TwCon
readTwConWithArity ss
 | Just n <- stripPrefix "Distinct" ss 
 , all isDigit n
 
 = Just (TwConDistinct $ read n)
 | otherwise = Nothing
 
 
-- | Read a builtin type constructor with a non-symbolic name.
--   ie not '->'.
readTcConBuiltin :: String -> Maybe TcCon
readTcConBuiltin ss
 = case ss of
        "Unit"          -> Just TcConUnit
        "S"             -> Just TcConSusp
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


-- | Read a witness constructor.
readWbConBuiltin :: String -> Maybe WbCon
readWbConBuiltin ss
 = case ss of
        "pure"          -> Just WbConPure
        "empty"         -> Just WbConEmpty
        "use"           -> Just WbConUse
        "read"          -> Just WbConRead
        "alloc"         -> Just WbConAlloc
        _               -> Nothing


-- Variable names -------------------------------------------------------------
-- | String is a variable name
isVarName :: String -> Bool
isVarName str
 = case str of
     []          -> False
     c : cs 
        | isVarStart c 
        , and (map isVarBody cs)
        -> True
        
        | _ : _         <- cs
        , Just initCs   <- takeInit cs
        , isVarStart c
        , and (map isVarBody initCs)
        , last cs == '#'
        -> True

        | otherwise
        -> False


-- | Charater can start a variable name.
isVarStart :: Char -> Bool
isVarStart = isLower
        

-- | Character can be part of a variable body.
isVarBody  :: Char -> Bool
isVarBody c
        =  isUpper c 
        || isLower c 
        || isDigit c 
        || c == '_' 
        || c == '\'' 
        || c == '$'


-- | Read a named, user defined variable.
readVar :: String -> Maybe String
readVar ss
        | isVarName ss  = Just ss
        | otherwise     = Nothing


-- Constructor names ----------------------------------------------------------
-- | String is a constructor name.
isConName :: String -> Bool
isConName str
 = case str of
     []          -> False
     c : cs 
        | isConStart c 
        , and (map isConBody cs)
        -> True
        
        | _ : _         <- cs
        , Just initCs   <- takeInit cs
        , isConStart c
        , and (map isConBody initCs)
        , last cs == '#'
        -> True

        | otherwise
        -> False

-- | Character can start a constructor name.
isConStart :: Char -> Bool
isConStart = isUpper


-- | Charater can be part of a constructor body.
isConBody  :: Char -> Bool
isConBody c     
        =  isUpper c 
        || isLower c 
        || isDigit c 
        || c == '_'
        

-- | Read a named, user defined `TcCon`.
readCon :: String -> Maybe String
readCon ss
        | isConName ss  = Just ss
        | otherwise     = Nothing


-- Operator names -------------------------------------------------------------
-- | String is the name of some operator.
isOpName :: String -> Bool
isOpName str
 = case str of
        []      -> False
        c : cs
         | isOpStart c
         , and (map isOpBody cs)
         -> True

         | otherwise
         -> False


-- | Character can start an operator.
isOpStart :: Char -> Bool
isOpStart c
        =  c == '~'     || c == '!'     || c == '@'     || c == '#'     
        || c == '$'     || c == '%'                     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'


-- | Character can be part of an operator body.
isOpBody :: Char -> Bool
isOpBody c
        =  c == '~'     || c == '!'     || c == '@'     || c == '#'     
        || c == '$'     || c == '%'     || c == '^'     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'


-- Literal names --------------------------------------------------------------
-- | String is the name of a literal.
isLitName :: String -> Bool
isLitName str
 = case str of
        []      -> False
        c : cs
         | isLitStart c
         , and (map isLitBody cs)
         -> True

         | otherwise
         -> False

-- | Character can start a literal.
isLitStart :: Char -> Bool
isLitStart c
        =   isDigit c
        ||  c == '-'

-- | Character can be part of a literal body.
isLitBody :: Char -> Bool
isLitBody c
        =  isDigit c
        || c == 'b' || c == 'o' || c == 'x'
        || c == 'w' || c == 'i' 
        || c == '#'
        || c == '\''

