
-- | Utilities for constructing and destructing compound expressions.
module DDC.Core.Compounds 
        ( module DDC.Type.Compounds
        , module DDC.Core.DaCon

          -- * Annotations
        , takeAnnotOfExp

          -- * Lambdas
        , makeXLAMs, takeXLAMs
        , makeXLams, takeXLams
        , makeXLamFlags
        , takeXLamFlags

          -- * Applications
        , makeXApps
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXConApps
        , takeXPrimApps

          -- * Lets
        , bindsOfLets
        , specBindsOfLets
        , valwitBindsOfLets
        , makeXLets
        , splitXLets 

          -- * Patterns
        , bindsOfPat

          -- * Alternatives
        , takeCtorNameOfAlt

          -- * Types
        , takeXType

          -- * Witnesses
        , takeXWitness
        , makeWApp
        , makeWApps
        , takeWAppsAsList
        , takePrimWiConApps

          -- * Units
        , xUnit)
where
import DDC.Type.Compounds
import DDC.Core.Exp
import DDC.Core.DaCon

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


-- Lambdas ---------------------------------------------------------------------
-- | Make some nested type lambdas.
makeXLAMs :: a -> [Bind n] -> Exp a n -> Exp a n
makeXLAMs a bs x
        = foldr (XLAM a) x (reverse bs)


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLAMs xx
 = let  go bs (XLAM _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested value or witness lambda.
makeXLams :: a -> [Bind n] -> Exp a n -> Exp a n
makeXLams a bs x
        = foldr (XLam a) x (reverse bs)


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
makeXApps   :: a -> Exp a n -> [Exp a n] -> Exp a n
makeXApps a t1 ts     = foldl (XApp a) t1 ts


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


-- Lets -----------------------------------------------------------------------
-- | Take the binds of a `Lets`.
--
--   The level-1 and level-0 binders are returned separately.
bindsOfLets :: Lets a n -> ([Bind n], [Bind n])
bindsOfLets ll
 = case ll of
        LLet _ b _         -> ([],  [b])
        LRec bxs           -> ([],  map fst bxs)
        LLetRegions bs bbs -> (bs, bbs)
        LWithRegion{}      -> ([],  [])


-- | Like `bindsOfLets` but only take the spec (level-1) binders.
specBindsOfLets :: Lets a n -> [Bind n]
specBindsOfLets ll
 = case ll of
        LLet _ _ _       -> []
        LRec _           -> []
        LLetRegions bs _ -> bs
        LWithRegion{}    -> []


-- | Like `bindsOfLets` but only take the value and witness (level-0) binders.
valwitBindsOfLets :: Lets a n -> [Bind n]
valwitBindsOfLets ll
 = case ll of
        LLet _ b _       -> [b]
        LRec bxs         -> map fst bxs
        LLetRegions _ bs -> bs
        LWithRegion{}    -> []


-- | Wrap some let-bindings around an expression.
makeXLets :: a -> [Lets a n] -> Exp a n -> Exp a n
makeXLets a lts x
 = foldr (XLet a) x lts


-- | Split let-bindings from the front of an expression, if any.
splitXLets :: Exp a n -> ([Lets a n], Exp a n)
splitXLets xx
 = case xx of
        XLet _ lts x 
         -> let (lts', x')      = splitXLets x
            in  (lts : lts', x')

        _ -> ([], xx)


-- Alternatives ---------------------------------------------------------------
-- | Take the constructor name of an alternative, if there is one.
takeCtorNameOfAlt :: Alt a n -> Maybe n
takeCtorNameOfAlt aa
 = case aa of
        AAlt (PData dc _) _     -> takeNameOfDaCon dc
        _                       -> Nothing


-- Patterns -------------------------------------------------------------------
-- | Take the binds of a `Pat`.
bindsOfPat :: Pat n -> [Bind n]
bindsOfPat pp
 = case pp of
        PDefault          -> []
        PData _ bs        -> bs


-- Types ----------------------------------------------------------------------
-- | Take the type from an `XType` argument, if any.
takeXType :: Exp a n -> Maybe (Type n)
takeXType xx
 = case xx of
        XType t -> Just t
        _       -> Nothing


-- Witnesses ------------------------------------------------------------------
-- | Take the witness from an `XWitness` argument, if any.
takeXWitness :: Exp a n -> Maybe (Witness n)
takeXWitness xx
 = case xx of
        XWitness t -> Just t
        _          -> Nothing


-- | Construct a witness application
makeWApp :: Witness n -> Witness n -> Witness n
makeWApp = WApp


-- | Construct a sequence of witness applications
makeWApps :: Witness n -> [Witness n] -> Witness n
makeWApps = foldl makeWApp


-- | Flatten an application into the function parts and arguments, if any.
takeWAppsAsList :: Witness n -> [Witness n]
takeWAppsAsList ww
 = case ww of
        WApp w1 w2 -> takeWAppsAsList w1 ++ [w2]
        _          -> [ww]


-- | Flatten an application of a witness into the witness constructor
--   name and its arguments.
--
--   Returns nothing if there is no witness constructor in head position.
takePrimWiConApps :: Witness n -> Maybe (n, [Witness n])
takePrimWiConApps ww
 = case takeWAppsAsList ww of
        WCon wc : args | WiConBound (UPrim n _) _ <- wc
          -> Just (n, args)
        _ -> Nothing


-- Units -----------------------------------------------------------------------
-- | Construct a value of unit type.
xUnit   :: a -> Exp a n
xUnit a = XCon a dcUnit

