
-- | Slurp out arities of functions, 
--   and infer arities for primitives based on their types.
module DDC.Core.Analysis.Arity
        ( Arities       (..)
        , emptyArities
        , extendsArities
        , getArity

          -- * Slurp
        , aritiesOfModule
        , arityOfLets
        , arityOfExp
        , arityFromType
        , aritiesOfPat)
where
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Compounds
import qualified Data.Map       as Map


-- | Arities of named and anonymous functions.
type Arities n = (Map.Map n Int, [Int])


-- | Empty arities context.
emptyArities :: Ord n => Arities n
emptyArities = (Map.empty, [])


-- | Extend map with multiple bindings and their arities.
extendsArities :: Ord n => Arities n -> [(Bind n, Int)] -> Arities n
extendsArities arity exts = foldl go arity exts
 where  go (named, anon) (BNone _t,   _) = (named,anon)
        go (named, anon) (BAnon _t,   a) = (named, a:anon)
        go (named, anon) (BName n _t, a) = (Map.insert n a named, anon)


-- | Look up a binder's arity.
getArity :: Ord n => Arities n -> Bound n -> Maybe Int
getArity (named, anon) u
 = case u of
        -- Get arities of anonymous things from the stack.
        UIx ix  _       -> Just (anon !! ix)

        -- Lookup arities of named things from the stack.
        UName n _       -> Map.lookup n named

        -- A hole has no expression, so no arity.
        UHole   _       -> Nothing

        -- Get a primitive's arity from its type.
        -- The arities of primitives always match their types, so this is ok.
        UPrim _ t       -> Just $ arityFromType t


-- | Get the arities of a `Lets`
arityOfLets :: Ord n => Lets a n -> [(Bind n, Int)]
arityOfLets ll
 = case ll of
        LLet _ b x      -> [(b, arityOfExp x)]
        LRec bxs        -> [(b, arityOfExp x) | (b, x) <- bxs ]
        _               -> []



-- Slurp ----------------------------------------------------------------------
-- | Slurp out arities of imports and top-level bindings from a module.
aritiesOfModule :: Ord n => Module a n -> Arities n
aritiesOfModule mm
  = let (lts, _)        = splitXLets $ moduleBody mm
        aritiesLets     = concatMap arityOfLets  lts

        aritiesImports  = [ (BName n t, arityFromType t)        
                          | (n, (_, t)) <- Map.toList $ moduleImportTypes mm ]

    in  emptyArities
        `extendsArities` aritiesImports
        `extendsArities` aritiesLets


-- | Get the arity of an expression. 
--   Count lambdas, use type for primitives.
arityOfExp :: Ord n => Exp a n -> Int
arityOfExp xx
 = case xx of
        -- Counting all binders, because they all correspond to XApps.
        XLam _ _ e              -> 1 + arityOfExp e
        XLAM _ _ e              -> 1 + arityOfExp e

        -- Find primitive's constructor's arities from type,
        -- we might need to do this for user defined constructors too.
        XCon _ (UPrim _ t)      -> arityFromType t

        -- Anything else we'll need to apply one at a time
        _                       -> 0


-- | Determine the arity of an expression by looking at its type.
--   Count all the function arrows, and foralls.
arityFromType :: Ord n => Type n -> Int
arityFromType tt
 = case tt of
        TForall _ t             
          -> 1 + arityFromType t

        t -> let (args, _) = takeTFunArgResult t
             in  length args


-- | Retrieve binders from case pattern, so we can extend the arity context.
--   We don't know anything about their values, so record as 0.
aritiesOfPat :: Ord n => Pat n -> [(Bind n, Int)]
aritiesOfPat p
 = case p of
        PDefault        -> []
        (PData _b bs)   -> zip bs (repeat 0)

