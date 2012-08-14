
module DDC.Core.Simplifier.Parser
        (parseSimplifier)
where
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Rewrite
import DDC.Core.Simplifier.Base
import DDC.Core.Exp
import DDC.Type.Env
import qualified DDC.Core.Simplifier.Recipe     as R
import Data.Char


-- | Parse a simplifier specification.
parseSimplifier 
        :: (Env n -> Namifier s n)      -- ^ Namifier for type variables.
        -> (Env n -> Namifier s n)      -- ^ Namifier for exp variables.
        -> [RewriteRule a n]            -- ^ Rewrite rule set.
        -> (n -> Maybe (Exp a n))       -- ^ Inliner templates.
        -> String 
        -> Maybe (Simplifier s a n)

parseSimplifier namK namT rules templates str
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
                "anormalize"    -> Just (R.anormalize namK namT)
                "rewriteSimp"	-> Just (R.rewriteSimp rules)
                _               -> Nothing

        parse1 k@KCon{}
         | Just t       <- parseTransform namK namT rules templates k
         = Just $ Trans t

        parse1 _        = Nothing


parseTransform 
        :: (Env n -> Namifier s n)      -- ^ Namifier for type variables.
        -> (Env n -> Namifier s n)      -- ^ Namifier for exp  variables.
        -> [RewriteRule a n]            -- ^ Rewrite rule set.
        -> (n -> Maybe (Exp a n))       -- ^ Inliner templates.
        -> Tok 
        -> Maybe (Transform s a n)

parseTransform namK namT rules templates (KCon name)
 = case name of
        "Id"            -> Just Id
        "Anonymize"     -> Just Anonymize
        "Snip"          -> Just Snip
        "Flatten"       -> Just Flatten
        "Beta"          -> Just Beta
        "Inline"        -> Just (Inline templates)
        "Namify"        -> Just (Namify namK namT)
        "Rewrite"       -> Just (Rewrite rules)
        _               -> Nothing

parseTransform _ _ _ _ _
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

