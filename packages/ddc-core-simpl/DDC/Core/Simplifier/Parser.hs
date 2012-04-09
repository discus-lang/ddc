
module DDC.Core.Simplifier.Parser
        (parseSimplifier)
where
import DDC.Core.Simplifier.Base
import qualified DDC.Core.Simplifier.Recipie    as R
import Data.Char


-- | Parse a simplifier specification.
parseSimplifier :: String -> Maybe Simplifier
parseSimplifier str
 = parse (lexSimplifier str) 
 where
        parse (k : KSemi : rest)
         | Just t1      <- parse1 k
         , Just t2      <- parse rest
         = Just $ Seq t1 t2

        parse (k : [])  = parse1 k
        parse _         = Nothing


        parse1 (KVar name)
         = case name of
                "anormalize"    -> Just R.anormalize
                _               -> Nothing

        parse1 k@KCon{}
         | Just t       <- parseTransform k
         = Just $ Trans t

        parse1 _        = Nothing

parseTransform :: Tok -> Maybe Transform
parseTransform (KCon name)
 = case name of
        "Id"            -> Just Id
        "Anonymize"     -> Just Anonymize
        "Snip"          -> Just Snip
        "Flatten"       -> Just Flatten
        "Beta"          -> Just Beta
        "Rewrite"       -> Just Rewrite
        "Namify"        -> Just Namify
        _               -> Nothing

parseTransform _        
 = Nothing


-- Lexer ----------------------------------------------------------------------
-- | Lex a transform specification.
lexSimplifier :: String -> [Tok]
lexSimplifier ss
 = case ss of
        []              -> []

        ('<' : '>' : cs)
         -> KSemi  : lexSimplifier cs

        (c : cs)
         | isSpace c
         -> lexSimplifier cs

        (c : cs)
         | isUpper c
         , (body, rest) <- span isAlpha cs
         -> KCon (c : body) : lexSimplifier rest

        (c : cs)
         | isLower c
         ,  (body, rest) <- span isAlpha cs
         -> KVar (c : body) : lexSimplifier rest

        _ -> [KJunk ss]


-- | Tokens for transform specification.
data Tok
        = KJunk         String
        | KCon          String
        | KVar          String
        | KSemi
        deriving (Eq, Show)

