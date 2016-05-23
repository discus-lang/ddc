{-# LANGUAGE TypeFamilies #-}

-- | Utilities for constructing and destructing Source Tetra expressions.
module DDC.Source.Tetra.Compounds
        ( module DDC.Type.Exp.Simple.Compounds
        , takeAnnotOfExp

          -- * Binds
        , bindOfBindMT
        , takeTypeOfBindMT

          -- * Lambdas
        , makeXLAMs
        , makeXLams
        , makeXLamFlags
        , takeXLAMs
        , takeXLams
        , takeXLamFlags

          -- * Applications
        , makeXApps
        , makeXAppsWithAnnots
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXAppsWithAnnots
        , takeXConApps
        , takeXPrimApps

          -- * Casts
        , makeXBox
        , makeXRun

          -- * Data Constructors
        , dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon

          -- * Patterns
        , bindsOfPat
        , makePTrue
        , makePFalse

          -- * Witnesses
        , wApp
        , wApps
        , takeXWitness
        , takeWAppsAsList
        , takePrimWiConApps

          -- * Primitives
        , makeXErrorDefault)
where
import DDC.Source.Tetra.Exp
import DDC.Source.Tetra.Prim
import DDC.Type.Exp.Simple.Compounds
import Data.Maybe
import Data.Text                        (Text)

import qualified DDC.Type.Exp           as T

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
        

-- Binds ----------------------------------------------------------------------
-- | Take the `GBind` of a `GBindMT`
bindOfBindMT :: GBindMT l -> GBind l
bindOfBindMT (BindMT g _mt) = g


-- | Take the type of a `GBindMT`.
takeTypeOfBindMT :: GBindMT l -> Maybe (T.Type (GName l))
takeTypeOfBindMT (BindMT _g mt) = mt


-- Annotations ----------------------------------------------------------------
-- | Take the outermost annotation from an expression,
--   or Nothing if this is an `XType` or `XWitness` without an annotation.
takeAnnotOfExp :: GExp l -> Maybe (GAnnot l)
takeAnnotOfExp xx
 = case xx of
        XAnnot a _      -> Just a
        XVar{}          -> Nothing
        XPrim{}         -> Nothing
        XCon{}          -> Nothing
        XLAM    _  x    -> takeAnnotOfExp x
        XLam    _  x    -> takeAnnotOfExp x
        XApp    x1 x2   -> firstJust $ map takeAnnotOfExp [x1, x2]
        XLet    _  x    -> takeAnnotOfExp x
        XCase   x  _    -> takeAnnotOfExp x
        XCast   _  x    -> takeAnnotOfExp x
        XType{}         -> Nothing
        XWitness{}      -> Nothing
        XDefix    a _   -> Just a
        XInfixOp  a _   -> Just a
        XInfixVar a _   -> Just a

firstJust = listToMaybe . catMaybes

-- Lambdas ---------------------------------------------------------------------
-- | Make some nested type lambdas.
makeXLAMs :: [GBindMT l] -> GExp l -> GExp l
makeXLAMs bs x = foldr XLAM x bs


-- | Make some nested value or witness lambdas.
makeXLams :: [GBindMT l] -> GExp l -> GExp l
makeXLams bs x = foldr XLam x bs


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: GExp l -> Maybe ([GBindMT l], GExp l)
takeXLAMs xx
 = let  go bs (XAnnot _ x) = go bs x
        go bs (XLAM b x)   = go (b:bs) x
        go bs x            = (reverse bs, x)

   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Split nested value or witness lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLams :: GExp l -> Maybe ([GBindMT l], GExp l)
takeXLams xx
 = let  go bs (XAnnot _ x) = go bs x
        go bs (XLam b x)   = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested lambda abstractions,
--   using a flag to indicate whether the lambda is a
--   level-1 (True), or level-0 (False) binder.
makeXLamFlags :: [(Bool, GBindMT l)] -> GExp l -> GExp l
makeXLamFlags fbs x
 = foldr (\(f, b) x'
           -> if f then XLAM b x'
                   else XLam b x')
                x fbs


-- | Split nested lambdas from the front of an expression, 
--   with a flag indicating whether the lambda was a level-1 (True), 
--   or level-0 (False) binder.
takeXLamFlags 
        :: GExp l 
        -> Maybe ([(Bool, GBindMT l)], GExp l)

takeXLamFlags xx
 = let  go bs (XAnnot _ x)  = go bs x
        go bs (XLAM b x)    = go ((True,  b):bs) x
        go bs (XLam b x)    = go ((False, b):bs) x
        go bs x             = (reverse bs, x)

   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Applications ---------------------------------------------------------------
-- | Build sequence of value applications.
makeXApps   :: GExp l -> [GExp l] -> GExp l
makeXApps t1 ts     = foldl XApp t1 ts


-- | Build sequence of applications.
--   Similar to `xApps` but also takes list of annotations for 
--   the `XApp` constructors.
makeXAppsWithAnnots :: GExp l -> [(GExp l, Maybe (GAnnot l))] -> GExp l
makeXAppsWithAnnots f xas
 = case xas of
        []                  -> f
        (arg, Nothing) : as -> makeXAppsWithAnnots (XApp f arg) as
        (arg, Just a)  : as -> makeXAppsWithAnnots (XAnnot a $ XApp f arg) as


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
        XAnnot _ x      -> takeXAppsAsList x
        XApp x1 x2      -> takeXAppsAsList x1 ++ [x2]
        _               -> [xx]


-- | Destruct sequence of applications.
--   Similar to `takeXAppsAsList` but also keeps annotations for later.
takeXAppsWithAnnots :: GExp l -> (GExp l, [(GExp l, Maybe (GAnnot l))])
takeXAppsWithAnnots xx
 = case xx of
        XAnnot a (XApp f arg)
         -> let (f', args') = takeXAppsWithAnnots f
            in  (f', args' ++ [(arg, Just a)])

        XApp f arg
         -> let (f', args') = takeXAppsWithAnnots f
            in  (f', args' ++ [(arg, Nothing)])

        _ -> (xx, [])


-- | Flatten an application of a primop into the variable
--   and its arguments.
--   
--   Returns `Nothing` if the expression isn't a primop application.
takeXPrimApps :: GExp l -> Maybe (GPrim l, [GExp l])
takeXPrimApps xx
 = case takeXAppsAsList xx of
        XPrim p : xs    -> Just (p, xs)
        _               -> Nothing

-- | Flatten an application of a data constructor into the constructor
--   and its arguments. 
--
--   Returns `Nothing` if the expression isn't a constructor application.
takeXConApps :: GExp l -> Maybe (DaCon (GName l) (Type (GName l)), [GExp l])
takeXConApps xx
 = case takeXAppsAsList xx of
        XCon dc : xs    -> Just (dc, xs)
        _               -> Nothing


-- Casts ----------------------------------------------------------------------
makeXBox x = XCast CastBox x
makeXRun x = XCast CastRun x


-- Primitives -----------------------------------------------------------------
makeXErrorDefault :: (GPrim l ~ PrimVal, GName l ~ Name)
              => Text -> Integer -> GExp l
makeXErrorDefault name n
 = makeXApps
        (XPrim (PrimValError OpErrorDefault))
        [ XCon (DaConPrim (NameLitTextLit name) (tBot kData))
        , XCon (DaConPrim (NameLitNat     n)    (tBot kData))]


-- Patterns -------------------------------------------------------------------
makePTrue    = PData (DaConPrim (NameLitBool True)  tBool) []
makePFalse   = PData (DaConPrim (NameLitBool False) tBool) []


