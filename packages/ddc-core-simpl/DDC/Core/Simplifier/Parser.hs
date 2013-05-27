
module DDC.Core.Simplifier.Parser
        ( SimplifierDetails     (..)
        , parseSimplifier)
where
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Inline
import DDC.Core.Simplifier.Base
import DDC.Core.Module
import DDC.Type.Env
import DDC.Core.Simplifier.Lexer
import DDC.Data.Token
import DDC.Data.SourcePos
import DDC.Base.Parser                          (pTok)
import Data.Set                                 (Set)
import qualified DDC.Core.Transform.Snip        as Snip
import qualified DDC.Core.Transform.Eta         as Eta
import qualified DDC.Base.Parser                as P
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


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

          -- | Modules available for inlining.
        , simplifierTemplates           :: [Module a n] }


-------------------------------------------------------------------------------
-- | A parser of simplifier specifications.
type Parser n a
        = P.Parser (Tok n) a

-- | Parse a simplifier from a string.
parseSimplifier
        :: (Ord n, Show n)
        => (String -> Maybe n)               -- Function to read a name.
        -> SimplifierDetails s a n
        -> String
        -> Either P.ParseError (Simplifier s a n)

parseSimplifier readName details str
 = let  kend    = Token KEnd (SourcePos "<simplifier spec>" 0 0)
        toks    = lexSimplifier readName str ++ [kend]
   in   P.runTokenParser show "<simplifier spec>" 
                (pSimplifier details)
                toks


-- | Parse a simplifier.
pSimplifier 
        :: (Ord n, Show n)
        => SimplifierDetails s a n
        -> Parser n (Simplifier s a n)

pSimplifier details
 = do   simpl   <- pSimplifierSeq details
        pTok KEnd
        return simpl


-- | Parse a simplifier sequence.
pSimplifierSeq 
        :: (Ord n, Show n)
        => SimplifierDetails s a n
        -> Parser n (Simplifier s a n)

pSimplifierSeq details
 = P.choice
 [ do   -- Single Transform or Sequence.
        simpl0  <- pSimplifier0 details

        P.choice
         [ do   pTok KSemiColon
                simpl1  <- pSimplifierSeq details
                return  $ Seq simpl0 simpl1

         , do   return simpl0 ]
 ]


pSimplifier0
        :: (Ord n, Show n)
        => SimplifierDetails s a n
        -> Parser n (Simplifier s a n)

pSimplifier0 details
 = P.choice
 [      -- Fixpoint transform.
        --  fix INT SIMP
   do   pTok KFix
        maxIters <- pInt
        simp     <- pSimplifier0 details
        return  $ Fix maxIters simp

 , do   -- Atomic transform.
        trans   <- pTransform details
        return  $ Trans trans

 , do   -- Simplifier in braces
        --  { SIMP }
        pTok KBraceBra
        simpl   <- pSimplifierSeq details
        pTok KBraceKet
        return simpl
 ]


-- | Parse a single transform.
pTransform 
        :: (Ord n, Show n)
        => SimplifierDetails s a n
        -> Parser n (Transform s a n)

pTransform details
 = P.choice
 [      -- Single transforms with no parameters.
   do   trans   <- P.pTokMaybe readTransformAtomic
        return trans

        -- Namifier
 , do   pTok (KCon "Namify")
        return  $ Namify (simplifierMkNamifierT details)
                         (simplifierMkNamifierX details)

        -- Rewrite
 , do   pTok (KCon "Rewrite")
        return  $ Rewrite (simplifierRules details) 

        -- Inline
 , do   pTok (KCon "Inline")
        let modules     = simplifierTemplates details
        specs           <- P.many pInlinerSpec
        let specsMap    = Map.fromList specs
        return  $ Inline (lookupTemplateFromModules specsMap modules) ]


-- | Parse an inlining specification.
pInlinerSpec 
        :: (Ord n, Show n)
        => Parser n (ModuleName, InlineSpec n)

pInlinerSpec 
 = P.choice 
 [ do   modname  <- pModuleName
        P.choice
         [ pInlinerSpecIncludeList modname
         , pInlinerSpecExcludeList modname
         , return (modname, InlineSpecAll modname (Set.empty :: Set n)) ]
 ]

-- Inline all bindings in a module, except particulars.
--   Inline MODULENAME +[VAR1, VAR2, ... VARn]
--   Inline MODULENAME  [VAR1, VAR2, ... VARn]
pInlinerSpecIncludeList modname
 = do   P.choice [ pTok KPlus, return () ]
        pTok KSquareBra
        ns      <- P.sepEndBy pVar (pTok KComma)
        pTok KSquareKet
        return  $ (modname, InlineSpecNone modname (Set.fromList ns))


-- Inline no bindings in a module by default,
--   but include some particulars.
--   Inline MODULENAME -[VAR1, VAR2, ... VARn]
pInlinerSpecExcludeList modname
 = do   pTok KMinus
        pTok KSquareBra
        ns      <- P.sepEndBy pVar (pTok KComma)
        pTok KSquareKet
        return  $ (modname, InlineSpecAll modname (Set.fromList ns))


-- | Read an atomic transform name.
readTransformAtomic :: Tok n -> Maybe (Transform s a n)
readTransformAtomic kk
 | KCon name  <- kk
 = case name of
        "Id"            -> Just Id
        "Anonymize"     -> Just Anonymize

        -- TODO: better parsing of snipper options.
        "Snip"          -> Just (Snip Snip.configZero)
        "SnipOver"      -> Just (Snip Snip.configZero { Snip.configSnipOverApplied = True })

        -- TODO: better parsing of eta options.
        "Eta"           -> Just (Eta  Eta.configZero  { Eta.configExpand = True })

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


-- | Parse a variable name
pVar :: Parser n n
pVar = P.pTokMaybe f
 where  f (KVar n) = Just n
        f _        = Nothing


-- | Parse an integer.
pInt :: Parser n Int
pInt = P.pTokMaybe f
 where  f (KInt i) = Just i
        f _        = Nothing


-- | Parse a module name.
pModuleName :: Parser n ModuleName
pModuleName = P.pTokMaybe f
 where  f (KCon n) = Just $ ModuleName [n]
        f _        = Nothing


