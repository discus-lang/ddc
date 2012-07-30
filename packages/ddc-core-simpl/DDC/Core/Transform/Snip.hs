
-- | Snip out nested applications.
module DDC.Core.Transform.Snip
        (Snip(..))
where
import DDC.Core.Analysis.Arity
import DDC.Core.Module
import DDC.Core.Exp
import qualified DDC.Core.Transform.LiftX       as L
import qualified DDC.Type.Compounds             as T


class Snip (c :: * -> *) where

 -- | Snip out nested applications as anonymous bindings.
 -- 
 -- @
 --      f (g x) (h y)
 --  ==> let ^ = g x in ^ = h y in f ^1 ^0
 -- @
 snip :: Ord n => c n -> c n


instance Snip (Module a) where
 snip mm
  = let arities = aritiesOfModule mm
        body'   = snipX arities (moduleBody mm) []
    in  mm { moduleBody = body'  }


instance Snip (Exp a) where
 snip x = snipX emptyArities x []


-- ANormal --------------------------------------------------------------------
-- | Convert an expression into A-normal form.
snipX 
        :: Ord n
        => Arities n    -- ^ Arities of functions in environment.
        -> Exp a n      -- ^ Expression to transform.
        -> [(Exp a n,a)]-- ^ Arguments being applied to current expression.
        -> Exp a n

-- For applications, remember the argument that the function is being 
-- applied to, and decend into the funciton part.
snipX ar (XApp a lhs rhs) args
 = let  -- Normalise the argument and add to the argument list.
        args' = (snipX ar rhs [], a) : args

        -- Decent into the function.
   in   snipX ar lhs args'

-- Non-applications.
-- If this expression is being applied to arguments then split it out into
-- its own binding, otherwise just decend into it.
snipX ar x args
 =  let x' = go x 
    in case args of
        -- if there are no args, we're done
        [] -> x'

        -- there are arguments. we must apply them.
        _  -> makeLets ar x' args

 where
        -- helper for descent
        down ars e 
         = snipX (extendsArities ar ars) e []

        -- The snipX function shouldn't have called us with an XApp.
        go XApp{}           
         = error "DDC.Core.Transform.ANormal.anormal: unexpected XApp"

        -- leafy constructors
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
        -- Split out non-atomic discriminants into their own bindings.
        go (XCase a e alts) 
         | isNormal e
         = let  e'      = down [] e 
                alts'   = map (\(AAlt pat ae) 
                              -> AAlt pat (down (aritiesOfPat pat) ae)) alts 
           in   XCase a e' alts'

         | otherwise
         = let  e'      = down [] e
                alts'   = [AAlt pat (down (aritiesOfPat pat) ae) | AAlt pat ae <- alts]

           in   XLet a  (LLet LetStrict (BAnon (T.tBot T.kData)) e')
                        (XCase a (XVar a $ UIx 0 (T.tBot T.kData)) 
                                (map (L.liftX 1) alts'))

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
 = go 0 f0Arity ((f0,annot):args) []
 where
        Just f0Arity    = findArity f0

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

        findArity (XVar _ b) 
         | Just arity   <- getArity ar b
         = Just (max arity 1)

        findArity x
         = Just (max (arityOfExp x) 1)


-- | Check if an expression needs a binding, or if it's simple enough to be
--   applied as-is.
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

