
module DDCI.Core.Pipeline.Transform
        ( Transform     (..)
        , Transform1    (..)
        , parseTransform
        , applyTransform
        , applyTransformX)
where
import DDC.Base.Pretty
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.ANormal
import DDC.Core.Transform.Flatten
import DDC.Core.Transform.Beta
import DDC.Core.Transform.Rewrite
import qualified DDC.Core.Transform.Rewrite.Rule as R
import DDC.Core.Eval.Name
import Data.Char


-- | Desription of the transforms to apply to a core program.
data Transform
        = TransformSeq Transform1 Transform
        | Transform1   Transform1 
        deriving (Eq, Show)

data Transform1
        = TransformId
        | TransformAnonymize
        | TransformANormal
        | TransformFlatten
        | TransformBeta
        | TransformRewrite
        deriving (Eq, Show)


instance Pretty Transform where
 ppr ss
  = case ss of
        TransformSeq t1 tRest 
         -> ppr t1 <+> semi <+> ppr tRest

        Transform1 t1
         -> ppr t1


instance Pretty Transform1 where
 ppr ss
  = case ss of
        TransformId             -> text "None"
        TransformAnonymize      -> text "Anonymize"
        TransformANormal        -> text "ANormal"
        TransformFlatten        -> text "Flatten"
        TransformBeta           -> text "Beta"
        TransformRewrite        -> text "Rewrite"


-- Parse ---------------------------------------------------------------------
-- | Parse a transform specification.
parseTransform :: String -> Maybe Transform
parseTransform str
 = parse (lexTransform str) 
 where
        parse (k : KSemi : rest)
         | Just t1      <- parse1 k
         , Just t2      <- parse rest
         = Just $ TransformSeq t1 t2

        parse (k : [])
         | Just t1      <- parse1 k
         = Just $ Transform1 t1

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


-- Lex ------------------------------------------------------------------------
-- | Tokens for transform specification.
data Tok
        = KJunk       String
        | KCon        String
        | KSemi
        deriving (Eq, Show)

-- | Lex a transform specification.
lexTransform :: String -> [Tok]
lexTransform ss
 = case ss of
        []              -> []

        (';' : cs)
         -> KSemi  : lexTransform cs

        (c : cs)
         | isSpace c
         -> lexTransform cs

        (c : cs)
         | isUpper c
         , (body, rest) <- span isAlpha cs
         -> KCon (c : body) : lexTransform rest

        _ -> [KJunk ss]


-- Apply ----------------------------------------------------------------------
applyTransform
        :: (Show a, Ord n)
        => Transform
        -> Module a n
        -> Module a n

applyTransform spec mm
 = case spec of
        TransformSeq t1 ts      
         -> applyTransform ts (applyTransform1 t1 mm)

        Transform1 t1
         -> applyTransform1 t1 mm

applyTransform1 spec mm
 = case spec of
        TransformId             -> mm
        TransformAnonymize      -> anonymizeX mm
        TransformANormal        -> anormalise mm
        TransformFlatten        -> flatten mm
        _                       -> error "applyTransform: finish me"


-- Exp ------------------------------------------------------------------------
-- TODO: turn application of rewrite rules into its own transform.
applyTransformX 
        :: (Show a, Ord Name)
        => Transform 
        -> [R.RewriteRule a Name] 
        -> Exp a Name 
        -> Exp a Name

applyTransformX spec rules xx
 = case spec of
        TransformSeq t1 ts
         -> applyTransformX ts rules (applyTransformX1 t1 rules xx)

        Transform1 t1
         -> applyTransformX1 t1 rules xx

applyTransformX1 spec rules xx
 = case spec of
        TransformId             -> xx
        TransformAnonymize      -> anonymizeX xx
        TransformANormal        -> anormalise xx
        TransformFlatten        -> flatten xx
        TransformBeta           -> betaReduce xx
        TransformRewrite        -> rewrite rules xx
