
module DDC.Core.Simplifier.Parser
        ( ModuleName(..)
        , InlinerTemplates
        , parseSimplifier)
where
import DDC.Base.Pretty
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


-- | The inliner templates for the given module.
type InlinerTemplates a n 
        = (n -> Maybe (Exp a n))


-- | Parse a simplifier specification.
--
--   The resulting simplifier specification captures auxilliary information
--   such as the list of rewrite rules, and inliner templates. Therefore, 
--   we need to supply all the possible auxilliary info when parsing
--   the simplifier specification.
--
parseSimplifier 
	:: (Eq n, Show n, Pretty n)
        => (Env n -> Namifier s n)              -- ^ Namifier for type variables.
        -> (Env n -> Namifier s n)              -- ^ Namifier for exp variables.
        -> [(String, RewriteRule a n)]          -- ^ Rewrite rule set.
        -> InlinerTemplates a n                 -- ^ Inliner templates for the current module.
        -> [(ModuleName, InlinerTemplates a n)] -- ^ Inliner templates from other modules.
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
	:: (Eq n, Show n, Pretty n)
        => (Env n -> Namifier s n)              -- ^ Namifier for type variables.
        -> (Env n -> Namifier s n)              -- ^ Namifier for exp  variables.
        -> [(String, RewriteRule a n)]          -- ^ Rewrite rule set.
        -> InlinerTemplates a n                 -- ^ Inliner templates for the current module.
        -> [(ModuleName, InlinerTemplates a n)]	-- ^ Inliner templates from other modules.
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
        "DeadCode"      -> ret DeadCode
        "Forward"       -> ret Forward
        "Namify"        -> ret (Namify namK namT)
        "Rewrite"       -> ret (Rewrite rules)
        "Inline"        -> parseInline rest
        "Elaborate"     -> ret Elaborate
        _               -> Nothing
 where
  ret t = Just (t, rest)

  -- Can inline a specific module except for some functions.
  -- "Inline Int -boxInt -unboxInt"
  parseInline (KCon mname : toks)
   | Just f	 <- P.lookup (ModuleName [mname]) modules
   , (f', rest') <- parseInlineArgs toks f
   = Just (Inline f', rest')

  parseInline toks
   = let (f', rest') = parseInlineArgs toks templates
     in  Just (Inline f', rest')

  parseInlineArgs (KMinus : KVar except : rest') f
    = parseInlineArgs rest'
	(\lname ->
	    -- TODO This is really stupid, need some way to create names
	    if   (show $ ppr lname) == except
	    then Nothing
	    else f lname)

  parseInlineArgs rest' f = (f, rest')


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

        ('-' : cs)
         -> KMinus    : lexSimplifier cs

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
	| KMinus
        | KSemi
	| KRoundBra
	| KRoundKet
        deriving (Eq, Show)

