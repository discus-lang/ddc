
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
        -> [(String, RewriteRule a n)]  -- ^ Rewrite rule set.
        -> (n -> Maybe (Exp a n))       -- ^ Inliner templates.
        -> String 
        -> Maybe (Simplifier s a n)

parseSimplifier namK namT rules templates str
 = let toks = (lexSimplifier str) 
   in  case parse toks of
       Just (t,[]) -> Just t
       _	   -> Nothing
 where
        parse toks
         | Just (t1, KSemi : rest) <- parse1 toks
         , Just (t2,rest')	   <- parse rest
         = Just (Seq t1 t2, rest')

	 | Just (t1, rest)	   <- parse1 toks
         = Just (t1, rest)
	 
	 | otherwise
	 = Nothing


	parse1 (KVar "fix" : KInt n : KRoundBra : rest)
	 | Just (t1, rest1)	      <- parse rest
	 , KRoundKet : rest2	      <- rest1
	 = Just (Fix n t1, rest2)

	parse1 (KVar name : rest)
         = case name of
                "anormalize"    -> Just (R.anormalize namK namT, rest)
                "rewriteSimp"	-> Just (R.rewriteSimp rules, rest)
                _               -> Nothing

        parse1 (k@KCon{} : rest)
         | Just t       <- parseTransform namK namT rules templates k
         = Just (Trans t, rest)

        parse1 _        = Nothing


parseTransform 
        :: (Env n -> Namifier s n)      -- ^ Namifier for type variables.
        -> (Env n -> Namifier s n)      -- ^ Namifier for exp  variables.
        -> [(String, RewriteRule a n)]  -- ^ Rewrite rule set.
        -> (n -> Maybe (Exp a n))       -- ^ Inliner templates.
        -> Tok 
        -> Maybe (Transform s a n)

parseTransform namK namT rules templates (KCon name)
 = case name of
        "Id"            -> Just Id
        "Anonymize"     -> Just Anonymize
        "Snip"          -> Just Snip
        "Flatten"       -> Just Flatten
        "Bubble"        -> Just Bubble
        "Beta"          -> Just Beta
        "Forward"       -> Just Forward
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

        ('(' : cs)
         -> KRoundBra : lexSimplifier cs

        (')' : cs)
         -> KRoundKet : lexSimplifier cs

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

        (c : cs)
         | isDigit c
	 , (digits, rest) <- span isDigit cs
         -> KInt (read (c:digits)) : lexSimplifier rest

        _ -> [KJunk ss]


-- | Tokens for transform specification.
data Tok
        = KJunk         String
        | KCon          String
	| KInt		Int
        | KVar          String
        | KSemi
	| KRoundBra
	| KRoundKet
        deriving (Eq, Show)

