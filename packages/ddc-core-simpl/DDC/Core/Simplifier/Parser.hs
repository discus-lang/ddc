
module DDC.Core.Simplifier.Parser
        (parseSimplifier)
where
import DDC.Core.Simplifier.Base
import Data.Char


-- | Parse a simplifier specification.
parseSimplifier :: String -> Maybe Simplifier
parseSimplifier str
 = parse (lexSimplifier str) 
 where
        parse (k : KSemi : rest)
         | Just t1      <- parse1 k
         , Just t2      <- parse rest
         = Just $ SimplifierSeq (SimplifierTrans t1) t2

        parse (k : [])
         | Just t1      <- parse1 k
         = Just $ SimplifierTrans t1

        parse _
         = Nothing

        parse1 (KCon name)
         = case name of
                "None"          -> Just TransformId
                "Anonymize"     -> Just TransformAnonymize
                "ANormal"       -> Just TransformANormal
                "Flatten"       -> Just TransformFlatten
                "Beta"          -> Just TransformBeta
                "Rewrite"       -> Just TransformRewrite
                _               -> Nothing

        parse1 _        
         = Nothing


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

        _ -> [KJunk ss]


-- | Tokens for transform specification.
data Tok
        = KJunk       String
        | KCon        String
        | KSemi
        deriving (Eq, Show)
