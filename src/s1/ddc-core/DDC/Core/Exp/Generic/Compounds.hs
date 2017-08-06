{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utilities for constructing and destructing compound expressions.
--
--   For the generic version of the AST.
--
module DDC.Core.Exp.Generic.Compounds
        ( module DDC.Type.Exp.Simple.Compounds

        -- * Abstractions
        , makeXAbs,     takeXAbs
        , makeXLAMs,    takeXLAMs
        , makeXLams,    takeXLams

        -- * Applications
        , makeXApps,    takeXApps,      splitXApps
        , takeXConApps
        , takeXPrimApps

        -- * Data Constructors
        , dcUnit
        , takeNameOfDaCon
        , takeTypeOfDaCon)
where
import DDC.Core.Exp.Generic.Exp
import DDC.Core.Exp.DaCon
import DDC.Type.Exp.Simple.Compounds
import DDC.Type.Exp.Simple.Exp
import Data.Maybe


-- Abstractions ---------------------------------------------------------------
-- | Make some nested abstractions.
makeXAbs  :: [GParam l] -> GExp l -> GExp l
makeXAbs as xx
 = foldr XAbs xx as


-- | Split type and value/witness abstractions from the front of an expression,
--   or `Nothing` if there aren't any.
takeXAbs  :: GExp l -> Maybe ([GParam l], GExp l)
takeXAbs xx
 = let  go as (XAbs a x)   = go (a : as) x
        go as x            = (reverse as, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (as, body)     -> Just (as, body)


-- | Make some nested type lambdas.
makeXLAMs :: [GBind l] -> GExp l -> GExp l
makeXLAMs bs x
        = foldr XLAM x bs


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: GExp l -> Maybe ([GBind l], GExp l)
takeXLAMs xx
 = let  go bs (XLAM b x)   = go (b : bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested value or witness lambdas.
makeXLams :: [GBind l] -> GExp l -> GExp l
makeXLams bs x
        = foldr XLam x bs


-- | Split nested value or witness lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLams :: GExp l -> Maybe ([GBind l], GExp l)
takeXLams xx
 = let  go bs (XLam b x)   = go (b : bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Applications ---------------------------------------------------------------
-- | Build sequence of applications.
makeXApps  :: GExp l -> [GArg l] -> GExp l
makeXApps t1 ts
        = foldl XApp t1 ts


-- | Flatten an application into the functional expression and its arguments,
--   or `Nothing if this is not an application.
takeXApps :: GExp l -> Maybe (GExp l, [GArg l])
takeXApps xx
 = case xx of
        XApp x1@XApp{} a2
         -> case takeXApps x1 of
                Just (f1, as1)  -> Just (f1, as1 ++ [a2])
                Nothing         -> Nothing

        XApp x1 a2
         -> Just (x1, [a2])

        _                       -> Nothing


-- | Flatten an application into a functional expression and its arguments,
--   or just return the expression with no arguments if this is not
--   an application.
splitXApps :: GExp l -> (GExp l, [GArg l])
splitXApps xx
 = fromMaybe (xx, []) $ takeXApps xx


-- | Flatten an application of a primitive operators into the operator itself
--   and its arguments, or `Nothing` if this is not an application of a
--   primitive.
takeXPrimApps :: GExp l -> Maybe (GPrim l, [GArg l])
takeXPrimApps xx
 = case xx of
        XApp (XPrim p) a2
         -> Just (p, [a2])

        XApp x1@XApp{} a2
         -> case takeXPrimApps x1 of
                Just (p, as1)   -> Just (p, as1 ++ [a2])
                _               -> Nothing

        _                       -> Nothing


-- | Flatten an application of a data constructor into the constructor itself
--   and its arguments, or `Nothing` if this is not an application of a
--   data constructor.
takeXConApps :: GExp l -> Maybe (DaCon l (Type l), [GArg l])
takeXConApps xx
 = case xx of
        XApp (XCon c) a2
         -> Just (c, [a2])

        XApp x1@XApp{} a2
         -> case takeXConApps x1 of
                Just (c, as1)   -> Just (c, as1 ++ [a2])
                _               -> Nothing

        _                       -> Nothing

