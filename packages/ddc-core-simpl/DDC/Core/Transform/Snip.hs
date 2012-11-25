
-- | Snip out nested applications.
module DDC.Core.Transform.Snip
        (Snip(..))
where
import DDC.Core.Analysis.Arity
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import qualified DDC.Core.Transform.LiftX       as L
import qualified DDC.Type.Compounds             as T


class Snip (c :: * -> *) where

 -- | Snip out nested applications as anonymous bindings.
 -- 
 -- @
 --      f (g x) (h y)
 --  ==> let ^ = g x in ^ = h y in f ^1 ^0
 -- @
 snip   :: Ord n 
        => Bool         -- ^ Introduce extra bindings for over-applied functions.
        -> c n 
        -> c n


instance Snip (Module a) where
 snip bOver mm
  = let arities = aritiesOfModule mm
        body'   = snipX bOver arities (moduleBody mm) []
    in  mm { moduleBody = body'  }


instance Snip (Exp a) where
 snip bOver x = snipX bOver emptyArities x []


-- | Convert an expression into A-normal form.
snipX 
        :: Ord n
        => Bool           -- ^ Introduce extra bindings for over-applied functions.
        -> Arities n      -- ^ Arities of functions in environment.
        -> Exp a n        -- ^ Expression to transform.
        -> [(Exp a n, a)] -- ^ Arguments being applied to current expression.
        -> Exp a n

snipX bOver arities x args
        -- For applications, remember the argument that the function is being 
        --   applied to, and decend into the function part.
        --   This unzips application nodes as we decend into the tree.
        | XApp a fun arg        <- x
        = snipX bOver arities fun $ (snipX bOver arities arg [], a) : args

        -- Some non-application node with no arguments.
        | null args
        = enterX bOver arities x

        -- Some non-application node being applied to arguments.
        | otherwise
        = buildNormalisedApp bOver arities (enterX bOver arities x) args

-- Enter into a non-application.
enterX bOver arities xx
 = let  down ars e 
         = snipX bOver (extendsArities arities ars) e []

   in case xx of
        -- The snipX function shouldn't have called us with an XApp.
        XApp{}           
         -> error "DDC.Core.Transform.Snip: snipX shouldn't give us an XApp"

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
         | isAtom e
         -> let  e'      = down [] e 
                 alts'   = map (\(AAlt pat ae) 
                               -> AAlt pat (down (aritiesOfPat pat) ae)) alts 
            in   XCase a e' alts'

         | otherwise
         -> let e'      = down [] e
                alts'   = [AAlt pat (down (aritiesOfPat pat) ae) | AAlt pat ae <- alts]

            in   XLet a (LLet LetStrict (BAnon (T.tBot T.kData)) e')
                        (XCase a (XVar a $ UIx 0) 
                                 (map (L.liftX 1) alts'))

        -- cast
        XCast a c e
         -> XCast a c (down [] e)


-- | Build an A-normalised application of some functional expression to 
--   its arguments. Atomic arguments are applied directly, while 
--   on-atomic arguments are bound via let-expressions, then the
--   associated let-bound variable is passed to the function.
buildNormalisedApp 
        :: Ord n
        => Bool            -- ^ Introduce extra bindings for over-applied functions.
        -> Arities n	   -- ^ environment, arities of bound variables
        -> Exp a n	   -- ^ function
        -> [(Exp a n,a)]   -- ^ arguments being applied to current expression
        -> Exp a n

buildNormalisedApp _bOver _  f0 [] = f0
buildNormalisedApp bOver arities f0 args@( (_, annot) : _)
 = make annot f0 args
 where
        tBot' = T.tBot T.kData

        -- Lookup the arity of the function.
        f0Arity    
         = case f0 of
                XVar _ b
                 | Just arity <- getArity arities b
                 -> max arity 1

                _ -> max (arityOfExp' f0) 1

        -- Make a normalised function application.
        make a xFun xsArgs

         -- The function part is already atomic.
         | isAtom xFun
         = buildNormalisedFunApp bOver a f0Arity xFun xsArgs

         -- The function part is not atomic, 
         --  so we need to add an outer-most let-binding for it.
         | otherwise
         = XLet a (LLet LetStrict (BAnon tBot') xFun)
                  (buildNormalisedFunApp bOver a f0Arity 
                               (XVar a (UIx 0)) 
                               [ (L.liftX 1 x, a') | (x, a') <- xsArgs])


-- | Build an A-normalised application of some functional expression to 
--   its arguments. Atomic arguments are applied directly, while 
--   on-atomic arguments are bound via let-expressions, then the
--   associated let-bound variable is passed to the function.
--
--   Unlike the `buildNormalisedFunApp` function above, this one
--   wants the function part to be normalised as well.
buildNormalisedFunApp
        :: Ord n
        => Bool           -- ^ Introduce extra bindings for over-applied functions.
        -> a              -- ^ Annotation to use.
        -> Int            -- ^ Arity of the function part.
        -> Exp a n        -- ^ Function part.
        -> [(Exp a n, a)] -- ^ Arguments to apply
        -> Exp a n

buildNormalisedFunApp bOver an funArity xFun xsArgs
 = let  tBot' = T.tBot T.kData

        -- Split arguments into the already atomic ones,
        --  and the ones we need to introduce let-expressions for.
        argss    = splitArgs xsArgs

        -- Collect up the new let-bindings.
        xsLets   = [ (x, a)  
                        | (_,    a, _, Just x) <- argss]

        -- The total number of new let-bindings.
        nLets    = length xsLets

        -- Lift indices in each binding over the bindings before it.
        xsLets'  = [ (L.liftX n x, a)
                        | (x, a)        <- xsLets
                        | (n :: Int)    <- [0..] ]

        -- Lift indices in the function over the bindings before it.
        xFun'    = L.liftX nLets xFun

        -- Collect up the new function arguments.
        --  If the argument was already atomic then we have to lift
        --  its indices past the new let bindings we're about to add.
        --  Otherwise it's a reference to one of the bindings directly.
        xsArgs'  = [if liftMe 
                        then (L.liftX nLets xArg, a)
                        else (xArg, a)
                        | (xArg, a, liftMe, _)      <- argss]

        -- Construct the new function application.
        xFunApps 

         -- If the function is over-applied then create an intermediate
         -- binding that saturates it, then apply the extra arguments
         -- separately.
         | bOver
         , length xsArgs' > funArity
         , (xsSat, xsOver)      <- splitAt funArity xsArgs'
         = XLet an (LLet LetStrict (BAnon tBot') 
                        (makeXAppsWithAnnots xFun' xsSat))
                   (makeXAppsWithAnnots 
                        (XVar an (UIx 0)) 
                        [ (L.liftX 1 x, a) | (x, a) <- xsOver ])

         -- Function has the correct number of arguments,
         -- or is partially applied.
         | otherwise
         = makeXAppsWithAnnots 
                xFun'
                xsArgs'              

        -- Wrap the function application in the let-bindings
        -- for its arguments.
   in   foldr (\(x, a) x' -> XLet a x x')
                xFunApps
                [ (LLet LetStrict (BAnon tBot') x, a) 
                        | (x, a) <- xsLets' ]


-- | Sort function arguments into either the atomic ones, 
--   or compound ones.
splitArgs 
        :: Ord n
        => [(Exp a n, a)] 
        -> [( Exp a n            -- Expression to use as the new argument.
            , a                  -- Annoation for the argument application.
            , Bool               -- Whether this argument was already atomic.
            , Maybe (Exp a n))]  -- New expression to let-bind.

splitArgs args
 = reverse $ go 0 $ reverse args
 where  
        go _n [] = []
        go n ((xArg, a) : xsArgs)
         | isAtom xArg
         = (xArg,           a, True,  Nothing)    : go n       xsArgs

         | otherwise
         = (XVar a (UIx n), a, False, Just xArg)  : go (n + 1) xsArgs


-- | Check if an expression needs a binding, or if it's simple enough to be
--   applied as-is.
isAtom :: Ord n => Exp a n -> Bool
isAtom xx
 = case xx of
        XVar{}          -> True
        XCon{}          -> True
        XType{}         -> True
        XWitness{}      -> True

        -- Casts are ignored by code generator, so we can leave them in if
        -- their subexpression is normal
        XCast _ _ x     -> isAtom x
        _               -> False


-- | Take the arity of an expression, 
--   returning 0 for XType and XWitness.
arityOfExp' :: Ord n => Exp a n -> Int
arityOfExp' xx
 = case arityOfExp xx of
        Nothing -> 0
        Just a  -> a

