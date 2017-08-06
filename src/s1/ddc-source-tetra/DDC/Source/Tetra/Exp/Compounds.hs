{-# OPTIONS_HADDOCK hide #-}
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
          -- ** Applications
        , makeXApps
        , makeXAppsWithAnnots
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXAppsWithAnnots
        , takeXConApps
        , takeXFragApps

          -- ** Arguments
        , takeRType
        , takeRTerm
        , takeRWitness
        , takeRImplicit

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
--        , takeXWitness
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
--        , takeXWitness
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
--   if there is one.
takeAnnotOfExp :: GExp l -> Maybe (GXAnnot l)
takeAnnotOfExp xx
 = case xx of
        XAnnot a _          -> Just a
        XPrim{}             -> Nothing
        XFrag{}             -> Nothing
        XVar{}              -> Nothing
        XCon{}              -> Nothing
        XAbs    _  x        -> takeAnnotOfExp x
        XApp    x1 a2       -> firstJust [takeAnnotOfExp x1, takeAnnotOfArg a2]
        XLet    _  x        -> takeAnnotOfExp x
        XCase   x  _        -> takeAnnotOfExp x
        XCast   _  x        -> takeAnnotOfExp x
        XDefix    a _       -> Just a
        XInfixOp  a _       -> Just a
        XInfixVar a _       -> Just a
        XMatch    a _ _     -> Just a
        XWhere    a _ _     -> Just a
        XAbsPat   a _ _ _ _ -> Just a
        XLamCase  a _       -> Just a

firstJust = listToMaybe . catMaybes


-- | Take the outermost annotation from an argument,
--   if there is one.
takeAnnotOfArg :: GArg l -> Maybe (GXAnnot l)
takeAnnotOfArg arg
 = case arg of
        RType _             -> Nothing
        RTerm x             -> takeAnnotOfExp x
        RWitness  _         -> Nothing
        RImplicit arg'      -> takeAnnotOfArg arg'


-- Applications ---------------------------------------------------------------
-- | Build sequence of value applications.
makeXApps   :: GExp l -> [GArg l] -> GExp l
makeXApps t1 ts     = foldl XApp t1 ts


-- | Build sequence of applications.
--   Similar to `xApps` but also takes list of annotations for
--   the `XApp` constructors.
makeXAppsWithAnnots :: GExp l -> [(GArg l, Maybe (GXAnnot l))] -> GExp l
makeXAppsWithAnnots f xas
 = case xas of
        []                  -> f
        (arg, Nothing) : as -> makeXAppsWithAnnots (XApp f arg) as
        (arg, Just a)  : as -> makeXAppsWithAnnots (XAnnot a $ XApp f arg) as


-- | Flatten an application into the function part and its arguments.
--
--   Returns `Nothing` if there is no outer application.
takeXApps :: GExp l -> Maybe (GExp l, [GArg l])
takeXApps xx
 = case takeXAppsAsList xx of
        (_,  [])        -> Nothing
        (x1, args)      -> Just (x1, args)


-- | Flatten an application into the function part and its arguments.
--
--   This is like `takeXApps` above, except we know there is at least one argument.
takeXApps1 :: GExp l -> GArg l -> (GExp l, [GArg l])
takeXApps1 x1 a2
 = case takeXApps x1 of
        Nothing          -> (x1,  [a2])
        Just (x11, a12s) -> (x11, a12s ++ [a2])


-- | Flatten an application into the function parts and arguments, if any.
takeXAppsAsList  :: GExp l -> (GExp l, [GArg l])
takeXAppsAsList xx
 = case xx of
        XAnnot _ x
          -> takeXAppsAsList x

        XApp x1 a2
          -> let (f', args') = takeXAppsAsList x1
             in  (f', args' ++ [a2])

        _ -> (xx, [])


-- | Destruct sequence of applications.
--   Similar to `takeXAppsAsList` but also keeps annotations for later.
takeXAppsWithAnnots :: GExp l -> (GExp l, [(GArg l, Maybe (GXAnnot l))])
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
takeXFragApps :: GExp l -> Maybe (GXFrag l, [GArg l])
takeXFragApps xx
 = case takeXAppsAsList xx of
        (XFrag p, args) -> Just (p, args)
        _               -> Nothing

-- | Flatten an application of a data constructor into the constructor
--   and its arguments.
--
--   Returns `Nothing` if the expression isn't a constructor application.
takeXConApps :: GExp l -> Maybe (DaCon (GXBoundCon l) (GType l), [GArg l])
takeXConApps xx
 = case takeXAppsAsList xx of
        (XCon dc, args) -> Just (dc, args)
        _               -> Nothing


-- Arguments ------------------------------------------------------------------
-- | Take the type of a type argument, if it is one.
takeRType :: GArg l -> Maybe (GType l)
takeRType aa
 = case aa of
        RType t         -> Just t
        _               -> Nothing


-- | Take a witness from an argument, if it is one.
takeRWitness :: GArg l -> Maybe (GWitness l)
takeRWitness aa
 = case aa of
        RWitness w      -> Just w
        _               -> Nothing


-- | Take a witness from an argument, if it is one.
takeRTerm :: GArg l -> Maybe (GExp l)
takeRTerm aa
 = case aa of
        RTerm x         -> Just x
        _               -> Nothing


-- | Unwrap an implicit argument, if this is one.
takeRImplicit :: GArg l -> Maybe (GArg l)
takeRImplicit arg
 = case arg of
        RImplicit arg'  -> Just arg'
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

