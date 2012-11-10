
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

snipX arities x args
        -- For applications, remember the argument that the function is being 
        --   applied to, and decend into the function part.
        --   This unzips application nodes as we decend into the tree.
        | XApp a fun arg        <- x
        = snipX arities fun $ (snipX arities arg [], a) : args

        -- Some non-application node with no arguments.
        | null args
        = enterX arities x

        -- Some non-application none being applied to arguments.
        | otherwise
        = makeLets arities (enterX arities x) args

-- Enter into a non-application.
enterX arities xx
 = let  down ars e 
         = snipX (extendsArities arities ars) e []

   in case xx of
        -- The snipX function shouldn't have called us with an XApp.
        XApp{}           
         -> error "DDC.Core.Transform.ANormal.anormal: unexpected XApp"

        -- leafy constructors
        XVar{}           -> xx
        XCon{}           -> xx
        XType{}          -> xx
        XWitness{}       -> xx

        -- lambdas
        XLAM a b e
         -> XLAM a b (down [(b,0)] e)

        XLam a b e
         -> XLam a b (down [(b,0)] e)

        -- non-recursive let
        XLet a (LLet m b x1) x2
         -> let x1' = down [] x1
                x2' = down [(b, arityOfExp' x1')] x2
            in  XLet a (LLet m b x1') x2'

        -- recursive let
        XLet a (LRec lets) x2
         -> let bs      = map fst lets 
                xs      = map snd lets 
                ars     = zip bs (map arityOfExp' xs) 
                xs'     = map (down ars) xs
                x2'     = down ars x2
            in  XLet a (LRec $ zip bs xs') x2' 

        -- letregion, just make sure we record bindings with dummy val.
        XLet a (LLetRegions b bs) x2
         -> let ars = zip bs (repeat 0) 
            in  XLet a (LLetRegions b bs) (down ars x2)

        -- withregion
        XLet a (LWithRegion b) z2
         -> XLet a (LWithRegion b) (down [] z2)

        -- case
        -- Split out non-atomic discriminants into their own bindings.
        XCase a e alts
         | isNormal e
         -> let  e'      = down [] e 
                 alts'   = map (\(AAlt pat ae) 
                              -> AAlt pat (down (aritiesOfPat pat) ae)) alts 
            in   XCase a e' alts'

         | otherwise
         -> let e'      = down [] e
                alts'   = [AAlt pat (down (aritiesOfPat pat) ae) | AAlt pat ae <- alts]

            in   XLet a  (LLet LetStrict (BAnon (T.tBot T.kData)) e')
                        (XCase a (XVar a $ UIx 0) 
                                (map (L.liftX 1) alts'))

        -- cast
        XCast a c e
         -> XCast a c (down [] e)


-- | Create lets for any non-trivial arguments
makeLets 
        :: Ord n
        => Arities n	   -- ^ environment, arities of bound variables
        -> Exp a n	   -- ^ function
        -> [(Exp a n,a)]   -- ^ arguments being applied to current expression
        -> Exp a n

makeLets _  f0 [] = f0
makeLets arities f0 args@((_,annot):_) 
 = go 0 f0Arity ((f0,annot):args) []
 where
        f0Arity    
         = case f0 of
                XVar _ b
                 | Just arity <- getArity arities b
                 -> max arity 1

                _ -> max (arityOfExp' f0) 1


        tBot = T.tBot T.kData

        -- out of arguments, create XApps out of leftovers
        go i _arf [] acc
         = mkApps i 0 acc

        -- ISSUE #278: Snip transform doesn't handle over-applications.
        -- 
        -- f is fully applied and we have arguments left to add:
        --	create let for intermediate result
        -- BROKEN: this produces the wrong debruijn indices.
        -- go i arf ((x, a) : xs) acc 
        --  |  length acc > arf
        --  =  XLet a (LLet LetStrict (BAnon tBot) (mkApps i 0 acc))
        --           (go i 1 ((x, a) : xs) [(XVar a $ UIx 0, a)])

        ---- application to variable, don't bother binding
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
         = XVar a $ UIx i

        -- apply this argument and recurse
        mkApps l i ((x,a):xs) 
         | isNormal x
         = XApp a (mkApps l i xs) (L.liftX l x)

        mkApps l i ((_,a):xs)
         = XApp a (mkApps l (i+1) xs) (XVar a $ UIx i)



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


arityOfExp' :: Ord n => Exp a n -> Int
arityOfExp' xx
 = case arityOfExp xx of
        Nothing -> 0 -- error $ "no arity"
        Just a  -> a
