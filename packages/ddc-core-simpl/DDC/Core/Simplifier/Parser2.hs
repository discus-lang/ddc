
module DDC.Core.Simplifier.Parser2
        ( SimplifierDetails     (..)
        , parseSimplifier)
where
import DDC.Core.Transform.Namify
import DDC.Core.Simplifier.Base
import DDC.Core.Module
import DDC.Type.Env
import DDC.Core.Simplifier.Lexer
import qualified DDC.Base.Parser                as P


-------------------------------------------------------------------------------
-- | Auxilliary information that may be used by a simplifier.
data SimplifierDetails s a n
        = SimplifierDetails
        { -- | Create a namifier to make fresh type (level-1) 
          --   names that don't conflict with any already in this environment.
          simplifierMkNamifierT         :: Env n -> Namifier s n

          -- | Create a namifier to make fresh value or witness (level-0) 
          --   names that don't conflict with any already in this environment.
        , simplifierMkNamifierX         :: Env n -> Namifier s n

          -- | Rewrite rules along with their names.
        , simplifierRules               :: NamedRewriteRules a n

          -- | Inliner templates from the current module
        , simplifierLocalTemplates      :: InlinerTemplates a n

          -- | Inliner templates from imported moodules
        , simplifierImportedTemplates   :: [(ModuleName, InlinerTemplates a n)] }


-------------------------------------------------------------------------------
-- | A parser of simplifier specifications.
type Parser n a
        = P.Parser Tok a

parseSimplifier
        :: SimplifierDetails s a n
        -> String
        -> Either P.ParseError (Simplifier s a n)


parseSimplifier _details str
 = let  toks    = lexSimplifier str
   in   P.runTokenParser show "<simplifier spec>" pSimplifier toks


pSimplifier :: Parser n (Simplifier s a n)
pSimplifier 
 = P.choice
 [      -- Single transform.
   do   trans   <- pTransform
        return  $ Trans trans
 ]


pTransform :: Parser n (Transform s a n)
pTransform
 = P.choice
 [      -- Transform with an atomic specification
   do   P.pTokMaybe readTransformAtomic 
 ]


readTransformAtomic :: Tok -> Maybe (Transform s a n)
readTransformAtomic kk
 | KTrans name  <- kk
 = case name of
        "Id"            -> Just Id
        "Anonymize"     -> Just Anonymize
        "Snip"          -> Just Snip
        "SnipOver"      -> Just SnipOver
        "Flatten"       -> Just Flatten
        "Beta"          -> Just Beta
        "BetaLets"      -> Just BetaLets
        "Prune"         -> Just Prune
        "Forward"       -> Just Forward
        "Bubble"        -> Just Bubble
        "Elaborate"     -> Just Elaborate
        _               -> Nothing

 | otherwise
 = Nothing


