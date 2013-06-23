
-- | Slurp out arities of function bindings.
--   and infer arities for primitives based on their types.
--
--   For function bindings the arity is the number of outer-most lambdas
--   in the definition. 
--
--   For primitives, the arity is the number of function
--   constructors in its type. 
--
module DDC.Core.Analysis.Arity
        ( -- * Arities map
          Arities
        , emptyArities
        , extendsArities
        , getArity

          -- * Arity analysis
        , aritiesOfModule
        , aritiesOfLets
        , aritiesOfPat
        , arityFromType
        , arityOfExp)
where
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Data.ListUtils
import Control.Monad
import Data.Maybe
import qualified Data.Map       as Map


-- | Arities of named and anonymous bindings.
type Arities n = (Map.Map n Int, [Int])


-- | Empty arities context.
emptyArities :: Ord n => Arities n
emptyArities = (Map.empty, [])


-- | Extend map with some binders and their arities.
extendsArities :: Ord n => Arities n -> [(Bind n, Int)] -> Arities n
extendsArities arity exts = foldl go arity exts
 where  go (named, anon) (BNone _t,   _) = (named,anon)
        go (named, anon) (BAnon _t,   a) = (named, a:anon)
        go (named, anon) (BName n _t, a) = (Map.insert n a named, anon)


-- | Look up a binder's arity from the arity map
--   or determine it from its type in the case of primops.
getArity :: Ord n => Arities n -> Bound n -> Maybe Int
getArity (named, anon) u
 = case u of
        -- Get arities of anonymous things from the stack.
        UIx ix 
         -> let Just x  = index anon ix
            in  Just x

        -- Lookup arities of named things from the stack.
        UName n         -> Map.lookup n named

        -- Get a primitive's arity from its type.
        -- The arities of primitives always match their types, so this is ok.
        UPrim _ t       -> arityFromType t


-- Slurp ----------------------------------------------------------------------
-- | Slurp out arities of imports and top-level bindings from a module.
aritiesOfModule :: Ord n => Module a n -> Arities n
aritiesOfModule mm
  = let (lts, _)        = splitXLets $ moduleBody mm

        aritiesLets     
         = concat $ catMaybes $ map aritiesOfLets  lts

        aritiesImports  
         = catMaybes
         $ [ case arityFromType t of
                Just a  -> Just (BName n t, a)
                Nothing -> Nothing
           | (n, (_, t)) <- Map.toList $ moduleImportTypes mm ]

    in  emptyArities
        `extendsArities` aritiesImports
        `extendsArities` aritiesLets


-- | Get the arities of a `Lets`
aritiesOfLets :: Ord n => Lets a n -> Maybe [(Bind n, Int)]
aritiesOfLets ll
 = let get (b, x)
        = case arityOfExp x of
                Nothing         -> Nothing
                Just a          -> Just (b, a)
   in case ll of
        LLet b x        -> sequence $ map get [(b, x)]
        LRec bxs        -> sequence $ map get bxs
        _               -> Just []


-- | Retrieve binders from case pattern, so we can extend the arity context.
--   We don't know anything about their values, so record as 0.
aritiesOfPat :: Ord n => Pat n -> [(Bind n, Int)]
aritiesOfPat p
 = case p of
        PDefault        -> []
        (PData _b bs)   -> zip bs (repeat 0)


-- | Get the arity of an expression. 
arityOfExp :: Ord n => Exp a n -> Maybe Int
arityOfExp xx
 = case xx of
        -- Counting all binders, because they all correspond to XApps.
        XLam _ _ e      -> liftM (+ 1) $ arityOfExp e
        XLAM _ _ e      -> liftM (+ 1) $ arityOfExp e

        -- Determine a data constructor's arity from its type.
        XCon _ dc       -> arityFromType (typeOfDaCon dc)

        -- Anything else we'll need to apply one at a time
        _               -> Just 0


-- | Determine the arity of an expression by looking at its type.
--   Count all the function arrows, and foralls.
arityFromType :: Ord n => Type n -> Maybe Int
arityFromType tt
        | TForall _ t   <- tt
        = case arityFromType t of
                Nothing -> Nothing
                Just a  -> Just (1 + a)

        | isBot tt
        = Nothing

        | (args, _)     <- takeTFunArgResult tt
        = Just (length args)

