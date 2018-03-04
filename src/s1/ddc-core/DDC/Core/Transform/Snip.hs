
-- | Snip out nested applications.
module DDC.Core.Transform.Snip
        ( Snip   (..)
        , Config (..)
        , configZero)
where
import DDC.Core.Analysis.Arity
import DDC.Core.Module
import DDC.Core.Exp.Annot
import qualified DDC.Core.Transform.BoundX      as L
import qualified DDC.Type.Exp.Simple            as T


-------------------------------------------------------------------------------
-- | Snipper configuration.
data Config
        = Config
        { -- | Introduce new bindings for over-applied functions.
          configSnipOverApplied :: Bool

          -- | Ensure the body of a let-expression is a variable.
        , configSnipLetBody     :: Bool

          -- | Treat lambda abstractions as atomic,
          --   and don't snip them.
        , configPreserveLambdas :: Bool
        }


-- | Snipper configuration with all flags set to False.
configZero :: Config
configZero
        = Config
        { configSnipOverApplied = False
        , configSnipLetBody     = False
        , configPreserveLambdas = False }


-------------------------------------------------------------------------------
-- | Class of things that can have things snipped out of them.
class Snip (c :: * -> *) where

 -- | Snip out nested applications as anonymous bindings.
 --
 -- @
 --      f (g x) (h y)
 --  ==> let ^ = g x in ^ = h y in f ^1 ^0
 -- @
 snip   :: Ord n => Config -> c n -> c n


instance Snip (Module a) where
 snip config mm
  = let arities = aritiesOfModule mm
        body'   = snipX config arities (moduleBody mm) []
    in  mm { moduleBody = body'  }


instance Snip (Exp a) where
 snip config x
  = snipX config emptyArities x []


-- | Convert an expression into A-normal form.
snipX   :: Ord n
        => Config               -- ^ Snipper configuration.
        -> Arities n            -- ^ Arities of functions in environment.
        -> Exp a n              -- ^ Expression to transform.
        -> [(Arg a n, a)]       -- ^ Arguments being applied to current expression.
        -> Exp a n

snipX config arities x args
        -- For applications, remember the argument that the function is being
        --   applied to, and decend into the function part.
        --   This unzips application nodes as we decend into the tree.
        | XApp a fun arg        <- x
        =  snipX config arities fun
        $ (snipA config arities arg, a) : args

        -- Some non-application node with no arguments.
        | null args
        = enterX config arities x

        -- Some non-application node being applied to arguments.
        | otherwise
        = let   x'      = enterX config arities x
          in    buildNormalisedApp config arities x' args


-- | Convert a function argument to A-normal form.
snipA   :: Ord n
        => Config               -- ^ Snipper configuration.
        -> Arities n            -- ^ Arities of functions in environment.
        -> Arg a n              -- ^ Argument to transform.
        -> Arg a n

snipA config arities aa
 = case aa of
        RType{}         -> aa
        RWitness{}      -> aa
        RTerm x         -> RTerm     $ snipX config arities x []
        RImplicit a     -> RImplicit $ snipA config arities a


-- Enter into a non-application.
enterX config arities xx
 = let  down ars e
         = snipX config (extendsArities arities ars) e []

   in case xx of
        -- The snipX function shouldn't have called us with an XApp.
        XApp{}
         -> error "ddc-core-simpl.Snip: snipX shouldn't give us an XApp"

        -- leafy constructors
        XVar{}                  -> xx
        XPrim{}                 -> xx
        XCon{}                  -> xx

        -- lambdas
        XAbs a (MType b)     e  -> XAbs a (MType b)     (down [(b,0)] e)
        XAbs a (MTerm b)     e  -> XAbs a (MTerm b)     (down [(b,0)] e)
        XAbs a (MImplicit b) e  -> XAbs a (MImplicit b) (down [(b,0)] e)

        -- non-recursive let
        XLet a (LLet b x1) x2
         -> let x1'     = down [] x1
                x2'     = snipLetBody config a
                        $ down [(b, arityOfExp' x1')] x2
            in  XLet a (LLet b x1') x2'

        -- recursive let
        XLet a (LRec lets) x2
         -> let bs      = map fst lets
                xs      = map snd lets
                ars     = zip bs (map arityOfExp' xs)
                xs'     = map (down ars) xs
                x2'     = snipLetBody config a $ down ars x2
            in  XLet a (LRec $ zip bs xs') x2'

        -- private, just make sure we record bindings with dummy val.
        XLet a (LPrivate b mt bs) x2
         -> let ars     = zip bs (repeat 0)
                x2'     = snipLetBody config a $ down ars x2
            in  XLet a (LPrivate b mt bs) x2'

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
                alts'   = [AAlt pat (down (aritiesOfPat pat) ae)
                                | AAlt pat ae <- alts]
                xBody'  = snipLetBody config a
                        $ XCase a (XVar a $ UIx 0)
                                  (map (L.liftX 1) alts')

            in  XLet a (LLet (BAnon (T.tBot T.kData)) e')
                       xBody'

        -- cast
        XCast a c e
         -> XCast a c (down [] e)


-- | Build an A-normalised application of some functional expression to
--   its arguments. Atomic arguments are applied directly, while
--   non-atomic arguments are bound via let-expressions, then the
--   associated let-bound variable is passed to the function.
buildNormalisedApp
        :: Ord n
        => Config          -- ^ Snipper config.
        -> Arities n       -- ^ environment, arities of bound variables
        -> Exp a n         -- ^ function
        -> [(Arg a n, a)]  -- ^ arguments being applied to current expression
        -> Exp a n

buildNormalisedApp _ _  f0 [] = f0
buildNormalisedApp config arities f0 args@( (_, annot) : _)
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
         = buildNormalisedFunApp config a f0Arity xFun xsArgs

         -- The function part is not atomic,
         --  so we need to add an outer-most let-binding for it.
         | otherwise
         = XLet a (LLet (BAnon tBot') xFun)
                  (snipLetBody config a
                    $ buildNormalisedFunApp config a f0Arity
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
        => Config         -- ^ Snipper configuration.
        -> a              -- ^ Annotation to use.
        -> Int            -- ^ Arity of the function part.
        -> Exp a n        -- ^ Function part.
        -> [(Arg a n, a)] -- ^ Arguments to apply
        -> Exp a n

buildNormalisedFunApp config an funArity xFun xsArgs
 = let  tBot' = T.tBot T.kData

        -- Split arguments into the already atomic ones,
        --  and the ones we need to introduce let-expressions for.
        argss    = splitArgs config xsArgs

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
         | configSnipOverApplied config
         , length xsArgs' > funArity
         , (xsSat, xsOver)      <- splitAt funArity xsArgs'
         = XLet an (LLet (BAnon tBot')
                         (makeXAppsWithAnnots xFun' xsSat))
                   (snipLetBody config an
                    $ makeXAppsWithAnnots
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
   in   case xsLets' of
         []     -> xFunApps
         _      -> foldr (\(x, a) x' -> XLet a x x')
                        (snipLetBody config an xFunApps)
                        [ (LLet (BAnon tBot') x, a)
                                | (x, a) <- xsLets' ]


-- | Sort function arguments into either the atomic ones,
--   or compound ones.
splitArgs
        :: Ord n => Config
        -> [( Arg a n, a)]
        -> [( Arg a n            -- Expression to use as the new argument.
            , a                  -- Annoation for the argument application.
            , Bool               -- Whether this argument was already atomic.
            , Maybe (Exp a n))]  -- New expression to let-bind.

splitArgs config args
 = reverse $ go 0 $ reverse args
 where
        go _n [] = []

        go n ((aArg, a) : asArgs)
         = case aArg of
                RType{}                 -> (aArg, a, True, Nothing) : go n asArgs
                RWitness{}              -> (aArg, a, True, Nothing) : go n asArgs
                RTerm x                 -> go_snip RTerm               n x a asArgs
                RImplicit (RTerm x)     -> go_snip (RImplicit . RTerm) n x a asArgs
                _                       -> error "ddc-core-simpl: splitArgs invalid arg"

        go_snip make n xArg a asArgs
         | isAtom xArg
         = (make xArg,             a, True,  Nothing)    : go n       asArgs

         | configPreserveLambdas config
         , isXLam xArg || isXLAM xArg
         = (make xArg,             a, True,  Nothing)    : go n       asArgs

         | otherwise
         = (make $ XVar a (UIx n), a, False, Just xArg)  : go (n + 1) asArgs


-- | If `snipLetResults` is set and this is not an atomic expression then
--   introduce a new binding for it.
snipLetBody :: Ord n => Config -> a -> Exp a n -> Exp a n
snipLetBody config a xx
        | configSnipLetBody config
        , not (isAtom xx)
        , not (isXLet xx)
        = let  tBot'   = T.tBot T.kData
          in   XLet a  (LLet (BAnon tBot') xx)
                       (XVar a (UIx 0))

        | otherwise
        = xx


-- | Check if an expression needs a binding, or if it's simple enough to be
--   applied as-is.
isAtom :: Ord n => Exp a n -> Bool
isAtom xx
 = case xx of
        XCon{}   -> True
        XPrim{}  -> True
        XVar{}   -> True

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

