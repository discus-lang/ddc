{-# LANGUAGE TypeFamilies #-}

-- | Utilities for constructing and destructing Source Tetra expressions.
module DDC.Source.Tetra.Exp.Compounds
        ( takeAnnotOfExp

          -- * Binds
        , bindOfBindMT
        , takeTypeOfBindMT

          -- * Types
          -- ** Type Applications
        , T.makeTApps,   T.takeTApps

          -- ** Sum Types
        , makeTBot

          -- ** Function Types
        , T.makeTFun,    T.makeTFuns,   T.makeTFuns',   (T.~>)
        , T.takeTFun,    T.takeTFuns,   T.takeTFuns'

          -- ** Forall Types
        , T.makeTForall, T.makeTForalls
        , T.takeTForall

          -- ** Exists Types
        , T.makeTExists, T.takeTExists

          -- ** Union types
        , T.takeTUnion
        , T.makeTUnions, T.takeTUnions
        , T.splitTUnionsOfKind

          -- * Terms
          -- ** Lambdas
        , makeXLAMs
        , makeXLams
        , makeXLamFlags
        , takeXLAMs
        , takeXLams
        , takeXLamFlags

          -- ** Applications
        , makeXApps
        , makeXAppsWithAnnots
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXAppsWithAnnots
        , takeXConApps
        , takeXPrimApps

          -- ** Clauses
        , bindOfClause

          -- ** Casts
        , pattern XRun
        , pattern XBox

          -- ** Data Constructors
        , dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon

          -- ** Patterns
        , bindsOfPat

          -- * Witnesses
        , wApp
        , wApps
        , takeXWitness
        , takeWAppsAsList
        , takePrimWiConApps)
where
import DDC.Source.Tetra.Exp.Generic
import Data.Maybe
import qualified DDC.Type.Exp.Generic.Compounds as T

import DDC.Core.Exp.Annot
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
bindOfBindMT :: GXBindVarMT l -> GXBindVar l
bindOfBindMT (XBindVarMT g _mt) = g


-- | Take the type of a `GBindMT`.
takeTypeOfBindMT :: GXBindVarMT l -> Maybe (GType l)
takeTypeOfBindMT (XBindVarMT _g mt) = mt


-- Types ----------------------------------------------------------------------
-- | Make an empty union type of the given kind.
makeTBot  :: GType l -> GType l
makeTBot k = TCon (TyConUnion k)



-- Annotations ----------------------------------------------------------------
-- | Take the outermost annotation from an expression,
--   or Nothing if this is an `XType` or `XWitness` without an annotation.
takeAnnotOfExp :: GExp l -> Maybe (GXAnnot l)
takeAnnotOfExp xx
 = case xx of
        XAnnot a _              -> Just a
        XVar{}                  -> Nothing
        XPrim{}                 -> Nothing
        XCon{}                  -> Nothing
        XLAM    _  x            -> takeAnnotOfExp x
        XLam    _  x            -> takeAnnotOfExp x
        XApp    x1 x2           -> firstJust $ map takeAnnotOfExp [x1, x2]
        XLet    _  x            -> takeAnnotOfExp x
        XCase   x  _            -> takeAnnotOfExp x
        XCast   _  x            -> takeAnnotOfExp x
        XType{}                 -> Nothing
        XWitness{}              -> Nothing
        XDefix    a _           -> Just a
        XInfixOp  a _           -> Just a
        XInfixVar a _           -> Just a
        XMatch    a _ _         -> Just a
        XWhere    a _ _         -> Just a
        XLamPat   a _ _ _       -> Just a
        XLamCase  a _           -> Just a


firstJust = listToMaybe . catMaybes

-- Lambdas ---------------------------------------------------------------------
-- | Make some nested type lambdas.
makeXLAMs :: [GXBindVarMT l] -> GExp l -> GExp l
makeXLAMs bs x = foldr XLAM x bs


-- | Make some nested value or witness lambdas.
makeXLams :: [GXBindVarMT l] -> GExp l -> GExp l
makeXLams bs x = foldr XLam x bs


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: GExp l -> Maybe ([GXBindVarMT l], GExp l)
takeXLAMs xx
 = let  go bs (XAnnot _ x) = go bs x
        go bs (XLAM b x)   = go (b:bs) x
        go bs x            = (reverse bs, x)

   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Split nested value or witness lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLams :: GExp l -> Maybe ([GXBindVarMT l], GExp l)
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
makeXLamFlags :: [(Bool, GXBindVarMT l)] -> GExp l -> GExp l
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
        -> Maybe ([(Bool, GXBindVarMT l)], GExp l)

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
makeXAppsWithAnnots :: GExp l -> [(GExp l, Maybe (GXAnnot l))] -> GExp l
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
takeXAppsWithAnnots :: GExp l -> (GExp l, [(GExp l, Maybe (GXAnnot l))])
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
takeXPrimApps :: GExp l -> Maybe (GXPrim l, [GExp l])
takeXPrimApps xx
 = case takeXAppsAsList xx of
        XPrim p : xs    -> Just (p, xs)
        _               -> Nothing

-- | Flatten an application of a data constructor into the constructor
--   and its arguments. 
--
--   Returns `Nothing` if the expression isn't a constructor application.
takeXConApps :: GExp l -> Maybe (DaCon (GXBoundCon l) (GType l), [GExp l])
takeXConApps xx
 = case takeXAppsAsList xx of
        XCon dc : xs    -> Just (dc, xs)
        _               -> Nothing


-- Clauses --------------------------------------------------------------------
-- | Take the binding variable of a clause.
bindOfClause :: GClause l -> GXBindVar l
bindOfClause cc
 = case cc of
        SSig _ b _                      -> b
        SLet _ (XBindVarMT b _) _ _     -> b


-- Casts ----------------------------------------------------------------------
pattern XBox x = XCast CastBox x
pattern XRun x = XCast CastRun x

