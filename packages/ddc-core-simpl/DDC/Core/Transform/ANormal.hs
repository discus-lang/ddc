
-- | Conversion to administrative-normal form.
module DDC.Core.Transform.ANormal
        (ANormalise(..))
where
import DDC.Core.Module
import DDC.Core.Exp
import qualified DDC.Core.Transform.LiftX       as L
import qualified DDC.Type.Exp                   as T
import qualified DDC.Type.Compounds             as T
import qualified Data.Map                       as Map


class ANormalise (c :: * -> *) where
 -- | Convert a thing to a-normal form.
 anormalise :: Ord n => c n -> c n


instance ANormalise (Module a) where
 anormalise mm
  = let arities  = concatMap arityOfLets    $ moduleLets mm
        arities' = extendsArities emptyArities arities
        ls       = map (anormalTop arities') $ moduleLets mm
    in  mm { moduleLets = ls }


instance ANormalise (Exp a) where
 anormalise x = anormalX emptyArities x []



-- ANormal --------------------------------------------------------------------
-- | Convert the expression in a top-level binding to A-normal form.
anormalTop
        :: Ord n
        => Arities n    -- ^ Arities of functions in environment.
        -> Lets a n
        -> Lets a n

anormalTop arities ll
 = case ll of
        LLet mode b x   -> LLet mode b $ anormalX arities x []
        LRec bxs        -> LRec [(b, anormalX arities x []) | (b, x) <- bxs]
        _               -> ll


-- | Convert an expression into A-normal form.
anormalX 
        :: Ord n
        => Arities n    -- ^ Arities of functions in environment.
        -> Exp a n      -- ^ Expression to transform.
        -> [(Exp a n,a)]-- ^ Arguments being applied to current expression.
        -> Exp a n

-- Application: just record argument and descend into function
anormalX ar (XApp a lhs rhs) args
 = let  -- normalise rhs and add to arguments.
        args' = (anormalX ar rhs [], a) : args

        -- descend into lhs, remembering all args.
   in   anormalX ar lhs args'


-- Anything other than application: if we're applied to arguments add bindings,
-- otherwise just recurse.
anormalX ar x args
 =  let x' = go x 
    in case args of
        -- if there are no args, we're done
        [] -> x'

        -- there are arguments. we must apply them.
        _  -> makeLets ar x' args

 where
        -- helper for descent
        down ars e 
         = anormalX (extendsArities ar ars) e []

        -- we know x isn't an app.
        go XApp{}           
         = error "DDC.Core.Transform.ANormal.anormal: unexpected XApp"

        -- leafy ones
        go XVar{}           = x
        go XCon{}           = x
        go XType{}          = x
        go XWitness{}       = x

        -- lambdas
        go (XLAM a b e) 
         = XLAM a b (down [(b,0)] e)

        go (XLam a b e) 
         = XLam a b (down [(b,0)] e)

        -- non-recursive let
        go (XLet a (LLet m b le) re) 
         = let le' = down [] le 
               re' = down [(b, arityOfExp le')] re 
           in  XLet a (LLet m b le') re'

        -- recursive let
        go (XLet a (LRec lets) re) 
         = let  bs      = map fst lets 
                es      = map snd lets 
                ars     = zip bs (map arityOfExp es) 
                es'     = map (down ars) es 
                re'     = down ars re
           in   XLet a (LRec $ zip bs es') re' 

        -- letregion, just make sure we record bindings with dummy val.
        go (XLet a (LLetRegion b bs) re) 
         = let ars = zip bs (repeat 0) 
           in  XLet a (LLetRegion b bs) (down ars re)

        -- withregion
        go (XLet a (LWithRegion b) re) 
         = XLet a (LWithRegion b) (down [] re)

        -- case
        go (XCase a e alts) 
         = let  e'      = down [] e 
                alts'   = map (\(AAlt pat ae) 
                              -> AAlt pat (down (aritiesOfPat pat) ae)) alts 
           in   XCase a e' alts'

        -- cast
        go (XCast a c e) 
         = XCast a c (down [] e)


-- | Create lets for any non-trivial arguments
makeLets 
        :: Ord n
        => Arities n	   -- ^ environment, arities of bound variables
        -> Exp a n	   -- ^ function
        -> [(Exp a n,a)]   -- ^ arguments being applied to current expression
        -> Exp a n
makeLets _  f0 [] = f0
makeLets ar f0 args@((_,annot):_) 
 = go 0 (findArity f0) ((f0,annot):args) []
 where
        tBot = T.tBot T.kData

        -- out of arguments, create XApps out of leftovers
        go i _arf [] acc
         = mkApps i 0 acc

        -- f is fully applied and we have arguments left to add:
        --	create let for intermediate result
        go i arf ((x,a):xs) acc 
         |  length acc > arf
         =  XLet a (LLet LetStrict (BAnon tBot) (mkApps i 0 acc))
                (go i 1 ((x,a):xs) [(XVar a $ UIx 0 tBot,a)])

        -- application to variable, don't bother binding
        go i arf ((x,a):xs) acc 
         | isNormal x
         =  go i arf xs ((x,a):acc)

        -- non-trivial argument, create binding
        go i arf ((x,a):xs) acc
         = XLet a (LLet LetStrict (BAnon tBot) (L.liftX i x))
                (go (i+1) arf xs ((x,a):acc))


        -- fold list into applications
        -- can't create empty app
        mkApps _ _ []
         = error "DDC.Core.Transform.ANormal.makeLets.mkApps: unexpected empty list"

        -- single element - this is the function
        mkApps l _ [(x,_)] 
         | isNormal x
         = L.liftX l x

        mkApps _ i [(_,a)]
         = XVar a $ UIx i tBot

        -- apply this argument and recurse
        mkApps l i ((x,a):xs) | isNormal x
         = XApp a (mkApps l i xs) (L.liftX l x)

        mkApps l i ((_,a):xs)
         = XApp a (mkApps l (i+1) xs) (XVar a $ UIx i tBot)

        findArity (XVar _ b) = max (getArity ar b) 1
        findArity x          = max (arityOfExp x)  1


-- | Check if an expression needs a binding, or if it's simple enough to be applied as-is
isNormal :: Ord n => Exp a n -> Bool
isNormal xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XType{}         -> True
        XWitness{}      -> True

        -- Casts are ignored by code generator, so we can leave them in if
        -- their subexpression is normal
        XCast _ _ x     -> isNormal x
        _               -> False


-- Arities --------------------------------------------------------------------
-- Recording arities of known values
-- So we can try to create apps to fully apply 

-- | Arities of known bound variables.
--   We need to track everything even if it's not a function to keep indices
--   correct. Just use zero for unknown/irrelevant
type Arities n = (Map.Map n Int, [Int])


-- | Empty arities context
emptyArities :: Ord n => Arities n
emptyArities = (Map.empty, [])


-- | Extend map with multiple bindings and their arities
extendsArities :: Ord n => Arities n -> [(Bind n, Int)] -> Arities n
extendsArities arity exts = foldl go arity exts
 where  go (named,anon) (BNone _t,   _) = (named,anon)
        go (named,anon) (BAnon _t,   a) = (named, a:anon)
        go (named,anon) (BName n _t, a) = (Map.insert n a named, anon)


-- | Look up a binder's arity
getArity :: Ord n => Arities n -> Bound n -> Int
getArity (_named, anon) (UIx ix _)   = anon !! ix
getArity (named, _anon) (UName n _)  = named Map.! n
-- Get a primitive's arity from its type
getArity (_named,_anon) (UPrim _ t)  = arityFromType t


-- | Get the arities of a `Lets`
arityOfLets :: Ord n => Lets a n -> [(Bind n, Int)]
arityOfLets ll
 = case ll of
        LLet _ b x      -> [(b, arityOfExp x)]
        LRec bxs        -> [(b, arityOfExp x) | (b, x) <- bxs ]
        _               -> []


-- | Get the arity of an expression. 
--   Count lambdas, use type for primitives.
arityOfExp :: Ord n => Exp a n -> Int
-- Counting all binders, because they all correspond to XApps.
arityOfExp (XLam _ _ e)
        = 1 + arityOfExp e

arityOfExp (XLAM _ _ e)
        = 1 + arityOfExp e

-- Find primitive's constructor's arities from type,
-- we might need to do this for user defined constructors too.
arityOfExp (XCon _ (UPrim _ t))
        = arityFromType t

-- Anything else we'll need to apply one at a time
arityOfExp _
        = 0


-- | Determine the arity of an expression by looking at its type.
--   Count all the function arrows, and foralls.
arityFromType :: Ord n => Type n -> Int
arityFromType (T.TForall _ t)
 =  1 + arityFromType t

arityFromType t
 = let (args, _) = T.takeTFunArgResult t
   in  length args


-- | Retrieve binders from case pattern, so we can extend the arity context.
--   We don't know anything about their values, so record as 0.
aritiesOfPat :: Ord n => Pat n -> [(Bind n, Int)]
aritiesOfPat PDefault = []
aritiesOfPat (PData _b bs) = zip bs (repeat 0)

