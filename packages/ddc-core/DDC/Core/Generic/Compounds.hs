{-# LANGUAGE TypeFamilies #-}

-- | Utilities for constructing and destructing compound expressions.
--
--   For the generic version of the AST.
--
module DDC.Core.Generic.Compounds
        ( module DDC.Type.Compounds

        -- * Abstractions
        , GAbs (..)
        , makeXAbs,     takeXAbs
        , makeXLAMs,    takeXLAMs
        , makeXLams,    takeXLams

        -- * Applications
        , makeXApps,    takeXApps
        , takeXAppsAsList
        , takeXConApps
        , takeXPrimApps)
where
import DDC.Core.Generic.Exp
import DDC.Core.Exp.DaCon
import DDC.Type.Compounds


-- Abstractions ---------------------------------------------------------------
data GAbs l
        = GAbsLAM (Bind l)
        | GAbsLam (Bind l)

-- | Make some nested abstractions.
makeXAbs  :: [GAbs l] -> GExp l -> GExp l
makeXAbs as xx
 = foldr mkAbs xx as
 where  mkAbs (GAbsLAM b) x = XLAM b x
        mkAbs (GAbsLam b) x = XLam b x


-- | Split type and value/witness abstractions from the front of an expression,
--   or `Nothing` if there aren't any.
takeXAbs  :: GExp l -> Maybe ([GAbs l], GExp l)
takeXAbs xx
 = let  go bs (XLAM b x)   = go (GAbsLAM b : bs) x
        go bs (XLam b x)   = go (GAbsLam b : bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested type lambdas.
makeXLAMs :: [Bind l] -> GExp l -> GExp l
makeXLAMs bs x
        = foldr XLAM x bs


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: GExp l -> Maybe ([Bind l], GExp l)
takeXLAMs xx
 = let  go bs (XLAM b x)   = go (b : bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested value or witness lambdas.
makeXLams :: [Bind l] -> GExp l -> GExp l
makeXLams bs x
        = foldr XLam x bs


-- | Split nested value or witness lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLams :: GExp l -> Maybe ([Bind l], GExp l)
takeXLams xx
 = let  go bs (XLam b x)   = go (b : bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Applications ---------------------------------------------------------------
-- | Build sequence of applications.
makeXApps  :: GExp l -> [GExp l] -> GExp l
makeXApps t1 ts
        = foldl XApp t1 ts


-- | Flatten an application into the functional expression and its arguments,
--   or `Nothing if this is not an application.
takeXApps :: GExp l -> Maybe (GExp l, [GExp l])
takeXApps xx
 = case takeXAppsAsList xx of
        (x1 : xsArgs)   -> Just (x1, xsArgs)
        _               -> Nothing


-- | Flatten an application into the functional expression and its arguments,
--   or just return the overall expression if this is not an application.
takeXAppsAsList :: GExp l -> [GExp l]
takeXAppsAsList xx
 = case xx of
        XApp x1 x2      -> takeXAppsAsList x1 ++ [x2]
        _               -> [xx]


-- | Flatten an application of a primitive operators into the operator itself
--   and its arguments, or `Nothing` if this is not an application of a
--   primitive.
takeXPrimApps :: GExp l -> Maybe (Prim l, [GExp l])
takeXPrimApps xx
 = case takeXAppsAsList xx of
        XPrim p : xs    -> Just (p, xs)
        _               -> Nothing


-- | Flatten an application of a data constructor into the constructor itself
--   and its arguments, or `Nothing` if this is not an application of a 
--   data constructor.
takeXConApps :: GExp l -> Maybe (DaCon l, [GExp l])
takeXConApps xx
 = case takeXAppsAsList xx of
        XCon dc : xs    -> Just (dc, xs)
        _               -> Nothing


