
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
import qualified DDC.Type.Transform    as T
import Data.List
import Data.Char


-- WiCon names ------------------------------------------------------------------------------------
-- | Read a `WiCon`.
readWiConBuiltin :: String -> Maybe WiCon
readWiConBuiltin ss
 = case ss of
        "pure"          -> Just WiConPure
        "empty"         -> Just WiConEmpty
        "const"         -> Just WiConConst
        "mutable"       -> Just WiConMutable
        "lazy"          -> Just WiConLazy
        "direct"        -> Just WiConDirect
        "read"          -> Just WiConRead
        "share"         -> Just WiConShare
        _               -> Nothing


---------------------------------------------------------------------------------------------------
-- | Lex a string into type tokens.
--   
--   * This is a conservative extension of the core type parser.
--   * If there are any `Nothing` elements in the returned list then there was a lexical error.
lexExp :: Ord n => (String -> n) -> String -> [Token (Tok n)]
lexExp mkName str
 = concatMap lexWord $ words str
 where 
-- TODO: maintain the real source position.
  mkToken t = Token t (SourcePos Nothing 0 0)

  lexWord w
   = case w of
        []              -> []        

        -- Function Constructors
        '~' : '>' : w'  -> mkToken KKindFun       : lexWord w'
        '-' : '>' : w'  -> mkToken KTypeFun       : lexWord w'
        '-' : '(' : w'  -> mkToken KTypeFunBra    : lexWord w'
        ')' : '>' : w'  -> mkToken KTypeFunKet    : lexWord w'

        -- Compound Parens
        '{'  : ':' : w' -> mkToken KBraceColonBra : lexWord w'
        ':'  : '}' : w' -> mkToken KBraceColonKet : lexWord w'
        '<'  : ':' : w' -> mkToken KAngleColonBra : lexWord w'
        ':'  : '>' : w' -> mkToken KAngleColonKet : lexWord w'

        -- Parens
        '('  : w'       -> mkToken KRoundBra      : lexWord w'
        ')'  : w'       -> mkToken KRoundKet      : lexWord w'
        '['  : w'       -> mkToken KSquareBra     : lexWord w'
        ']'  : w'       -> mkToken KSquareKet     : lexWord w'
        '{'  : w'       -> mkToken KBraceBra      : lexWord w'
        '}'  : w'       -> mkToken KBraceKet      : lexWord w'
        '<'  : w'       -> mkToken KAngleBra      : lexWord w'
        '>'  : w'       -> mkToken KAngleKet      : lexWord w'            

        -- Punctuation
        '.'  : w'       -> mkToken KDot           : lexWord w'
        '|'  : w'       -> mkToken KBar           : lexWord w'
        '^'  : w'       -> mkToken KHat           : lexWord w'
        '+'  : w'       -> mkToken KPlus          : lexWord w'
        ':'  : w'       -> mkToken KColon         : lexWord w'
        ','  : w'       -> mkToken KComma         : lexWord w'
        '\\' : w'       -> mkToken KBackSlash     : lexWord w'
        ';'  : w'       -> mkToken KSemiColon     : lexWord w'
        '_'  : w'       -> mkToken KUnderscore    : lexWord w'
        
        -- Bottoms
        '!' : '0' : w'  -> mkToken KBotEffect     : lexWord w'
        '$' : '0' : w'  -> mkToken KBotClosure    : lexWord w'

        -- Sort Constructors
        '*' : '*' : w'  -> mkToken KSortComp      : lexWord w'
        '@' : '@' : w'  -> mkToken KSortProp      : lexWord w'        

        -- Kind Constructors
        '*' : w'        -> mkToken KKindValue     : lexWord w'
        '%' : w'        -> mkToken KKindRegion    : lexWord w'
        '!' : w'        -> mkToken KKindEffect    : lexWord w'
        '$' : w'        -> mkToken KKindClosure   : lexWord w'
        '@' : w'        -> mkToken KKindWitness   : lexWord w'
        
        -- Literal values
        c : cs
         | isDigit c
         , (body, rest)         <- span isDigit cs
         -> mkToken (KInteger (read (c:body))) : lexWord rest
        
        
        -- Named Constructors
        c : cs
         | isConStart c
         , (body, rest)         <- span isConBody cs
         -> let readNamedCon s
                 | Just twcon   <- readTwConBuiltin s
                 = mkToken (KTwConBuiltin twcon) : lexWord rest
                 
                 | Just tccon   <- readTcConBuiltin s
                 = mkToken (KTcConBuiltin $ T.rename mkName tccon) : lexWord rest
                 
                 | Just con     <- readCon s
                 = mkToken (KCon $ mkName con)    : lexWord rest
               
                 | otherwise    = [mkToken (KJunk s)]
                 
            in  readNamedCon (c : body)

        -- Named Variables and Witness constructors
        c : cs
         | isVarStart c
         , (body, rest)         <- span isVarBody cs
         -> let readNamedVar s
                 | Just wc      <- readWiConBuiltin s
                 = mkToken (KWiConBuiltin wc) : lexWord rest
         
                 | Just v       <- readVar s
                 = mkToken (KVar $ mkName v)   : lexWord rest

                 | otherwise    = [mkToken (KJunk s)]
            in  readNamedVar (c:body)

        -- Keywords
        _
         | Just (key, tok)        <- find (\(key, _) -> isPrefixOf key w) keywords
         -> mkToken tok : lexWord (drop (length key) w)
         
        -- Error
        _               -> [mkToken (KJunk w)]
        

-- | Textual keywords in the core language.
keywords :: [(String, Tok n)]
keywords
 =      [ ("in",     KIn)
        , ("of",     KOf) 
        , ("let",    KLet)
        , ("letrec", KLetRec)
        , ("local",  KLocal)
        , ("case",   KCase)
        , ("purify", KPurify)
        , ("forget", KForget) ]

