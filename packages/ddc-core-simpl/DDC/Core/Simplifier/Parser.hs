
module DDC.Core.Simplifier.Parser
        (parseSimplifier)
where
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Rewrite
import DDC.Core.Simplifier.Base
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Type.Env
import Data.Char
import qualified DDC.Core.Simplifier.Recipe	as R
import Prelude
import qualified Prelude			as P

type ModuleTemplate a n = (ModuleName, (n -> Maybe (Exp a n)))

-- | Parse a simplifier specification.
parseSimplifier 
        :: (Env n -> Namifier s n)      -- ^ Namifier for type variables.
        -> (Env n -> Namifier s n)      -- ^ Namifier for exp variables.
        -> [(String, RewriteRule a n)]  -- ^ Rewrite rule set.
        -> (n -> Maybe (Exp a n))       -- ^ Inliner templates.
        -> [ModuleTemplate a n]		-- ^ Module-specific inliner templates.
        -> String 
        -> Maybe (Simplifier s a n)

parseSimplifier namK namT rules templates module_templates str
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

        parse1 toks@(KCon{} : _)
         | Just (t,rest) <- parseTransform namK namT rules templates module_templates toks
         = Just (Trans t, rest)

        parse1 _        = Nothing


parseTransform 
        :: (Env n -> Namifier s n)      -- ^ Namifier for type variables.
        -> (Env n -> Namifier s n)      -- ^ Namifier for exp  variables.
        -> [(String, RewriteRule a n)]  -- ^ Rewrite rule set.
        -> (n -> Maybe (Exp a n))       -- ^ Inliner templates.
        -> [ModuleTemplate a n]		-- ^ Module-specific inliner templates.
        -> [Tok]
        -> Maybe (Transform s a n, [Tok])

parseTransform namK namT rules templates modules ((KCon name):rest)
 = case name of
        "Id"            -> ret Id
        "Anonymize"     -> ret Anonymize
        "Snip"          -> ret Snip
        "Flatten"       -> ret Flatten
        "Beta"          -> ret Beta
        "BetaLets"      -> ret BetaLets
        "Bubble"        -> ret Bubble
        "Forward"       -> ret Forward
        "Namify"        -> ret (Namify namK namT)
        "Rewrite"       -> ret (Rewrite rules)

        "Inline"        
	  -> case rest of
		(KCon mname : rest')
		  -> fmap (\t -> (Inline t, rest'))
			  (P.lookup (ModuleName [mname]) modules)
		_ -> ret (Inline templates)

        _               -> Nothing
 where
  ret t = Just (t, rest)

parseTransform _ _ _ _ _ _
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

