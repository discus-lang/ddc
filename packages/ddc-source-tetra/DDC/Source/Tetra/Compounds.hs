
module DDC.Source.Tetra.Compounds
        ( module DDC.Type.Compounds
        , takeAnnotOfExp

          -- * Lambdas
        , xLAMs
        , xLams
        , makeXLamFlags
        , takeXLAMs
        , takeXLams
        , takeXLamFlags

          -- * Applications
        , xApps
        , makeXAppsWithAnnots
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXAppsWithAnnots
        , takeXConApps
        , takeXPrimApps

          -- * Data Constructors
        , dcUnit
        , mkDaConAlg
        , mkDaConSolid
        , takeNameOfDaCon
        , typeOfDaCon

          -- * Patterns
        , bindsOfPat

          -- * Witnesses
        , wApp
        , wApps
        , takeXWitness
        , takeWAppsAsList
        , takePrimWiConApps)
where
import DDC.Source.Tetra.Exp
import DDC.Type.Compounds
import DDC.Core.Compounds
        ( dcUnit
        , mkDaConAlg
        , mkDaConSolid
        , takeNameOfDaCon
        , typeOfDaCon

        , bindsOfPat

        , wApp
        , wApps
        , takeXWitness
        , takeWAppsAsList
        , takePrimWiConApps)
        
-- Annotations ----------------------------------------------------------------
-- | Take the outermost annotation from an expression,
--   or Nothing if this is an `XType` or `XWitness` without an annotation.
takeAnnotOfExp :: Exp a n -> Maybe a
takeAnnotOfExp xx
 = case xx of
        XVar  a _       -> Just a
        XCon  a _       -> Just a
        XLAM  a _ _     -> Just a
        XLam  a _ _     -> Just a
        XApp  a _ _     -> Just a
        XLet  a _ _     -> Just a
        XCase a _ _     -> Just a
        XCast a _ _     -> Just a
        XType{}         -> Nothing
        XWitness{}      -> Nothing
        XDefix a _      -> Just a
        XInfixOp  a _   -> Just a
        XInfixVar a _   -> Just a


-- Lambdas ---------------------------------------------------------------------
-- | Make some nested type lambdas.
xLAMs :: a -> [Bind n] -> Exp a n -> Exp a n
xLAMs a bs x
        = foldr (XLAM a) x bs


-- | Make some nested value or witness lambdas.
xLams :: a -> [Bind n] -> Exp a n -> Exp a n
xLams a bs x
        = foldr (XLam a) x bs


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLAMs xx
 = let  go bs (XLAM _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Split nested value or witness lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLams :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLams xx
 = let  go bs (XLam _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested lambda abstractions,
--   using a flag to indicate whether the lambda is a
--   level-1 (True), or level-0 (False) binder.
makeXLamFlags :: a -> [(Bool, Bind n)] -> Exp a n -> Exp a n
makeXLamFlags a fbs x
 = foldr (\(f, b) x'
           -> if f then XLAM a b x'
                   else XLam a b x')
                x fbs


-- | Split nested lambdas from the front of an expression, 
--   with a flag indicating whether the lambda was a level-1 (True), 
--   or level-0 (False) binder.
takeXLamFlags :: Exp a n -> Maybe ([(Bool, Bind n)], Exp a n)
takeXLamFlags xx
 = let  go bs (XLAM _ b x) = go ((True,  b):bs) x
        go bs (XLam _ b x) = go ((False, b):bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Applications ---------------------------------------------------------------
-- | Build sequence of value applications.
xApps   :: a -> Exp a n -> [Exp a n] -> Exp a n
xApps a t1 ts     = foldl (XApp a) t1 ts


-- | Build sequence of applications.
--   Similar to `xApps` but also takes list of annotations for 
--   the `XApp` constructors.
makeXAppsWithAnnots :: Exp a n -> [(Exp a n, a)] -> Exp a n
makeXAppsWithAnnots f xas
 = case xas of
        []              -> f
        (arg,a ) : as   -> makeXAppsWithAnnots (XApp a f arg) as


-- | Flatten an application into the function part and its arguments.
--
--   Returns `Nothing` if there is no outer application.
takeXApps :: Exp a n -> Maybe (Exp a n, [Exp a n])
takeXApps xx
 = case takeXAppsAsList xx of
        (x1 : xsArgs)   -> Just (x1, xsArgs)
        _               -> Nothing


-- | Flatten an application into the function part and its arguments.
--
--   This is like `takeXApps` above, except we know there is at least one argument.
takeXApps1 :: Exp a n -> Exp a n -> (Exp a n, [Exp a n])
takeXApps1 x1 x2
 = case takeXApps x1 of
        Nothing          -> (x1,  [x2])
        Just (x11, x12s) -> (x11, x12s ++ [x2])


-- | Flatten an application into the function parts and arguments, if any.
takeXAppsAsList  :: Exp a n -> [Exp a n]
takeXAppsAsList xx
 = case xx of
        XApp _ x1 x2    -> takeXAppsAsList x1 ++ [x2]
        _               -> [xx]


-- | Destruct sequence of applications.
--   Similar to `takeXAppsAsList` but also keeps annotations for later.
takeXAppsWithAnnots :: Exp a n -> (Exp a n, [(Exp a n, a)])
takeXAppsWithAnnots xx
 = case xx of
        XApp a f arg
         -> let (f', args') = takeXAppsWithAnnots f
            in  (f', args' ++ [(arg,a)])

        _ -> (xx, [])


-- | Flatten an application of a primop into the variable
--   and its arguments.
--   
--   Returns `Nothing` if the expression isn't a primop application.
takeXPrimApps :: Exp a n -> Maybe (n, [Exp a n])
takeXPrimApps xx
 = case takeXAppsAsList xx of
        XVar _ (UPrim p _) : xs -> Just (p, xs)
        _                       -> Nothing

-- | Flatten an application of a data constructor into the constructor
--   and its arguments. 
--
--   Returns `Nothing` if the expression isn't a constructor application.
takeXConApps :: Exp a n -> Maybe (DaCon n, [Exp a n])
takeXConApps xx
 = case takeXAppsAsList xx of
        XCon _ dc : xs  -> Just (dc, xs)
        _               -> Nothing
