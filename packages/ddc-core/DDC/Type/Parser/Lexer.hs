
-- | Reference lexer for the type parser. Slow but Simple.
module DDC.Type.Parser.Lexer
        ( -- * Constructors
          isConName, isConStart, isConBody
        , readTwConBuiltin
        , readTcConBuiltin
        , readCon
        
          -- * Variables
        , isVarName, isVarStart, isVarBody
        , readVar
        
          -- * Lexer
        , lexType)
where
import DDC.Base.Lexer
import DDC.Type.Exp
import DDC.Type.Parser.Tokens
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


---------------------------------------------------------------------------------------------------
-- | Lex a string into type tokens.
--   If there are any `Nothing` elements in the returned list then there was a lexical error.
lexType :: Ord n => (String -> n) -> String -> [Token (Tok n)]
lexType mkName str
 = concatMap lexWord $ words str
 where 
  -- TODO: maintain the real source position.
  mkToken t = Token t (SourcePos Nothing 0 0)

  lexWord w
   = case w of
        []              -> []        

        -- Function Constructors
        '~' : '>' : w'  -> mkToken KKindFun     : lexWord w'
        '-' : '>' : w'  -> mkToken KTypeFun     : lexWord w'
        '-' : '(' : w'  -> mkToken KTypeFunBra  : lexWord w'
        ')' : '>' : w'  -> mkToken KTypeFunKet  : lexWord w'

        -- Brackets
        '(' : w'        -> mkToken KRoundBra    : lexWord w'
        ')' : w'        -> mkToken KRoundKet    : lexWord w'
        '[' : w'        -> mkToken KSquareBra   : lexWord w'
        ']' : w'        -> mkToken KSquareKet   : lexWord w'
        ':' : w'        -> mkToken KColon       : lexWord w'
        '.' : w'        -> mkToken KDot         : lexWord w'
        ',' : w'        -> mkToken KComma       : lexWord w'

        -- Sort Constructors
        '*' : '*' : w'  -> mkToken KSortComp    : lexWord w'
        '@' : '@' : w'  -> mkToken KSortProp    : lexWord w'        
        
        -- Bottoms
        '!' : '0' : w'  -> mkToken KBotEffect   : lexWord w'
        '$' : '0' : w'  -> mkToken KBotClosure  : lexWord w'

        -- Kind Constructors
        '*' : w'        -> mkToken KKindValue   : lexWord w'
        '%' : w'        -> mkToken KKindRegion  : lexWord w'
        '!' : w'        -> mkToken KKindEffect  : lexWord w'
        '$' : w'        -> mkToken KKindClosure : lexWord w'
        '@' : w'        -> mkToken KKindWitness : lexWord w'
        
        -- Symbolic Type Constructors
        '+' : w'        -> mkToken KPlus        : lexWord w'

        -- Named Constructors
        c : cs
         | isConStart c
         , (body,  rest)        <- span isConBody cs
         , (body', rest')       <- case rest of
                                        '#' : rest'     -> (body ++ "#", rest')
                                        _               -> (body, rest)
         -> let readNamedCon s
                 | Just twcon   <- readTwConBuiltin s
                 = mkToken (KTwConBuiltin twcon) : lexWord rest'
                 
                 | Just tccon   <- readTcConBuiltin s
                 = mkToken (KTcConBuiltin tccon) : lexWord rest'
                 
                 | Just con     <- readCon s
                 = mkToken (KCon $ mkName con)   : lexWord rest'
               
                 | otherwise    = [mkToken (KJunk s)]
                 
            in  readNamedCon (c : body')
        
        -- Named Variables
        c : cs
         | isVarStart c
         , (body, rest)         <- span isVarBody cs
         -> let readTyVar s
                 | Just v       <- readVar s
                 = mkToken (KVar $ mkName v)   : lexWord rest

                 | otherwise    = [mkToken (KJunk s)]
            in  readTyVar (c:body)

        -- Error
        _               -> [mkToken (KJunk w)]
 
