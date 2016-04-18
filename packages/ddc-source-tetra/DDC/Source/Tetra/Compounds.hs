{-# LANGUAGE TypeFamilies #-}

-- | Utilities for constructing and destructing Source Tetra expressions.
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

          -- * Casts
        , xBox
        , xRun

          -- * Data Constructors
        , dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon

          -- * Patterns
        , bindsOfPat
        , pTrue
        , pFalse

          -- * Witnesses
        , wApp
        , wApps
        , takeXWitness
        , takeWAppsAsList
        , takePrimWiConApps)
where
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.Prim
import DDC.Type.Compounds
import DDC.Core.Exp.Annot.Compounds
        ( dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon

        , bindsOfPat

        , wApp
        , wApps
        , takeXWitness
        , takeWAppsAsList
        , takePrimWiConApps)
        

-- Annotations ----------------------------------------------------------------
-- | Take the outermost annotation from an expression,
--   or Nothing if this is an `XType` or `XWitness` without an annotation.
takeAnnotOfExp :: GExp l -> Maybe (GAnnot l)
takeAnnotOfExp xx
 = case xx of
        XVar  a _       -> Just a
        XPrim a _       -> Just a
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
xLAMs :: GAnnot l -> [GBind l] -> GExp l -> GExp l
xLAMs a bs x
        = foldr (XLAM a) x bs


-- | Make some nested value or witness lambdas.
xLams :: GAnnot l -> [GBind l] -> GExp l -> GExp l
xLams a bs x
        = foldr (XLam a) x bs


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: GExp l -> Maybe ([GBind l], GExp l)
takeXLAMs xx
 = let  go bs (XLAM _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Split nested value or witness lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLams :: GExp l -> Maybe ([GBind l], GExp l)
takeXLams xx
 = let  go bs (XLam _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested lambda abstractions,
--   using a flag to indicate whether the lambda is a
--   level-1 (True), or level-0 (False) binder.
makeXLamFlags :: GAnnot l -> [(Bool, GBind l)] -> GExp l -> GExp l
makeXLamFlags a fbs x
 = foldr (\(f, b) x'
           -> if f then XLAM a b x'
                   else XLam a b x')
                x fbs


-- | Split nested lambdas from the front of an expression, 
--   with a flag indicating whether the lambda was a level-1 (True), 
--   or level-0 (False) binder.
takeXLamFlags :: GExp l -> Maybe ([(Bool, GBind l)], GExp l)
takeXLamFlags xx
 = let  go bs (XLAM _ b x) = go ((True,  b):bs) x
        go bs (XLam _ b x) = go ((False, b):bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Applications ---------------------------------------------------------------
-- | Build sequence of value applications.
xApps   :: GAnnot l -> GExp l -> [GExp l] -> GExp l
xApps a t1 ts     = foldl (XApp a) t1 ts


-- | Build sequence of applications.
--   Similar to `xApps` but also takes list of annotations for 
--   the `XApp` constructors.
makeXAppsWithAnnots :: GExp l -> [(GExp l, GAnnot l)] -> GExp l
makeXAppsWithAnnots f xas
 = case xas of
        []              -> f
        (arg,a ) : as   -> makeXAppsWithAnnots (XApp a f arg) as


-- | Flatten an application into the function part and its arguments.
--
--   Returns `Nothing` if there is no outer application.
takeXApps :: GExp l -> Maybe (GExp l, [GExp l])
takeXApps xx
 = case takeXAppsAsList xx of
        (x1 : xsArgs)   -> Just (x1, xsArgs)
        _               -> Nothing


-- | Flatten an application into the function part and its arguments.
--
--   This is like `takeXApps` above, except we know there is at least one argument.
takeXApps1 :: GExp l -> GExp l -> (GExp l, [GExp l])
takeXApps1 x1 x2
 = case takeXApps x1 of
        Nothing          -> (x1,  [x2])
        Just (x11, x12s) -> (x11, x12s ++ [x2])


-- | Flatten an application into the function parts and arguments, if any.
takeXAppsAsList  :: GExp l -> [GExp l]
takeXAppsAsList xx
 = case xx of
        XApp _ x1 x2    -> takeXAppsAsList x1 ++ [x2]
        _               -> [xx]


-- | Destruct sequence of applications.
--   Similar to `takeXAppsAsList` but also keeps annotations for later.
takeXAppsWithAnnots :: GExp l -> (GExp l, [(GExp l, GAnnot l)])
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
takeXPrimApps :: GExp l -> Maybe (GPrim l, [GExp l])
takeXPrimApps xx
 = case takeXAppsAsList xx of
        XPrim _ p : xs  -> Just (p, xs)
        _               -> Nothing

-- | Flatten an application of a data constructor into the constructor
--   and its arguments. 
--
--   Returns `Nothing` if the expression isn't a constructor application.
takeXConApps :: GExp l -> Maybe (DaCon (GName l), [GExp l])
takeXConApps xx
 = case takeXAppsAsList xx of
        XCon _ dc : xs  -> Just (dc, xs)
        _               -> Nothing


-- Casts ----------------------------------------------------------------------
xBox a x = XCast a CastBox x
xRun a x = XCast a CastRun x


-- Patterns -------------------------------------------------------------------
pTrue    = PData (DaConPrim (NameLitBool True)  tBool) []
pFalse   = PData (DaConPrim (NameLitBool False) tBool) []


