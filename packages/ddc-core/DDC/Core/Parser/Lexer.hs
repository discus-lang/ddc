
-- | Reference lexer for core langauge parser. Slow but Simple.
module DDC.Core.Parser.Lexer
        ( -- * Constructors
          isConName, isConStart, isConBody
        , readTwConBuiltin
        , readTcConBuiltin
        , readWiConBuiltin
        , readCon
        
          -- * Variables
        , isVarName, isVarStart, isVarBody
        , readVar

          -- * Lexer
        , lexExp)
where
import DDC.Base.Lexer
import DDC.Core.Exp
import DDC.Core.Parser.Tokens
import DDC.Type.Parser.Lexer
import Data.List
import Data.Char


-- WiCon names ----------------------------------------------------------------
-- | Read a `WiCon`.
---
--   This should function should have a case for every `WiCon`.
--
readWiConBuiltin :: String -> Maybe WiCon
readWiConBuiltin ss
 = case ss of
        "pure"          -> Just WiConPure
        "empty"         -> Just WiConEmpty
        "global"        -> Just WiConGlobal
        "const"         -> Just WiConConst
        "mutable"       -> Just WiConMutable
        "lazy"          -> Just WiConLazy
        "manifest"      -> Just WiConManifest
        "use"           -> Just WiConUse
        "read"          -> Just WiConRead
        "alloc"         -> Just WiConAlloc
        _               -> Nothing                                              -- TODO: add distinctN


-------------------------------------------------------------------------------
-- | Lex a string into type tokens.
--   
--   * This is a conservative extension of the core type parser.
--   * If there are any `Nothing` elements in the returned list then there was
--     a lexical error.
lexExp :: String -> [Token (Tok String)]
lexExp str
 = concatMap lexWord $ words str
 where 
  -- make a token
  tok t = Token t (SourcePos Nothing 0 0)                                       -- TODO: maintain real sourcepos
  tokA  = tok . KA
  tokN  = tok . KN

  lexWord w
   = case w of
        []              -> []        

        -- The unit data constructor
        '(' : ')' : w'  -> tokN (KCon "()")    : lexWord w'

        -- Function Constructors
        '~'  : '>'  : w' -> tokA KArrowTilde    : lexWord w'
        '-'  : '>'  : w' -> tokA KArrowDash     : lexWord w'
        '='  : '>'  : w' -> tokA KArrowEquals   : lexWord w'
        '-'  : '('  : w' -> tokA KTypeFunBra    : lexWord w'
        ')'  : '>'  : w' -> tokA KTypeFunKet    : lexWord w'

        -- Compound Parens
        '['  : ':' : w' -> tokA KSquareColonBra : lexWord w'
        ':'  : ']' : w' -> tokA KSquareColonKet : lexWord w'
        '<'  : ':' : w' -> tokA KAngleColonBra : lexWord w'
        ':'  : '>' : w' -> tokA KAngleColonKet : lexWord w'

        -- Compound symbols
        ':'  : ':' : w' -> tokA KHasType       : lexWord w'

        -- Debruijn indices
        '^'  : cs
         |  (ds, rest)   <- span isDigit cs
         ,  length ds >= 1
         -> tokA (KIndex (read ds)) : lexWord rest         

        -- Parens
        '('  : w'       -> tokA KRoundBra      : lexWord w'
        ')'  : w'       -> tokA KRoundKet      : lexWord w'
        '['  : w'       -> tokA KSquareBra     : lexWord w'
        ']'  : w'       -> tokA KSquareKet     : lexWord w'
        '{'  : w'       -> tokA KBraceBra      : lexWord w'
        '}'  : w'       -> tokA KBraceKet      : lexWord w'
        '<'  : w'       -> tokA KAngleBra      : lexWord w'
        '>'  : w'       -> tokA KAngleKet      : lexWord w'            

        -- Punctuation
        '.'  : w'       -> tokA KDot           : lexWord w'
        '|'  : w'       -> tokA KBar           : lexWord w'
        '^'  : w'       -> tokA KHat           : lexWord w'
        '+'  : w'       -> tokA KPlus          : lexWord w'
        ':'  : w'       -> tokA KColon         : lexWord w'
        ','  : w'       -> tokA KComma         : lexWord w'
        '\\' : w'       -> tokA KBackSlash     : lexWord w'
        ';'  : w'       -> tokA KSemiColon     : lexWord w'
        '_'  : w'       -> tokA KUnderscore    : lexWord w'
        '='  : w'       -> tokA KEquals        : lexWord w'
        '&'  : w'       -> tokA KAmpersand     : lexWord w'
        
        -- Bottoms
        '!' : '0' : w'  -> tokA KBotEffect     : lexWord w'
        '$' : '0' : w'  -> tokA KBotClosure    : lexWord w'

        -- Sort Constructors
        '*' : '*' : w'  -> tokA KSortComp      : lexWord w'
        '@' : '@' : w'  -> tokA KSortProp      : lexWord w'        

        -- Kind Constructors
        '*' : w'        -> tokA KKindValue     : lexWord w'
        '%' : w'        -> tokA KKindRegion    : lexWord w'
        '!' : w'        -> tokA KKindEffect    : lexWord w'
        '$' : w'        -> tokA KKindClosure   : lexWord w'
        '@' : w'        -> tokA KKindWitness   : lexWord w'
        
        -- Literal values
        c : cs
         | isDigit c
         , (body, rest)         <- span isDigit cs
         -> tokN (KLit (c:body)) : lexWord rest
        
        -- Named Constructors
        c : cs
         | isConStart c
         , (body,  rest)        <- span isConBody cs
         , (body', rest')       <- case rest of
                                        '#' : rest'     -> (body ++ "#", rest')
                                        _               -> (body, rest)
         -> let readNamedCon s
                 | Just twcon   <- readTwConBuiltin s
                 = tokA (KTwConBuiltin twcon) : lexWord rest'
                 
                 | Just tccon   <- readTcConBuiltin s
                 = tokA (KTcConBuiltin tccon) : lexWord rest'
                 
                 | Just con     <- readCon s
                 = tokN (KCon con)    : lexWord rest'
               
                 | otherwise    = [tok (KJunk s)]
                 
            in  readNamedCon (c : body')

        -- Keywords
        _
         | Just (key, t) <- find (\(key, _) -> isPrefixOf key w) keywords
         -> tok t : lexWord (drop (length key) w)

        -- Named Variables and Witness constructors
        c : cs
         | isVarStart c
         , (body, rest)         <- span isVarBody cs
         -> let readNamedVar s
                 | Just wc      <- readWiConBuiltin s
                 = tokA (KWiConBuiltin wc) : lexWord rest
         
                 | Just v       <- readVar s
                 = tokN (KVar v)   : lexWord rest

                 | otherwise    = [tok (KJunk s)]
            in  readNamedVar (c:body)

        -- Error
        _               -> [tok $ KJunk w]
        

-- | Textual keywords in the core language.
keywords :: [(String, Tok n)]
keywords
 =      [ ("in",         KA KIn)
        , ("of",         KA KOf) 
        , ("letrec",     KA KLetRec)
        , ("letregion",  KA KLetRegion)
        , ("withregion", KA KWithRegion)
        , ("let",        KA KLet)
        , ("case",       KA KCase)
        , ("purify",     KA KPurify)
        , ("forget",     KA KForget) 
        , ("with",       KA KWith)
        , ("where",      KA KWhere) ]
