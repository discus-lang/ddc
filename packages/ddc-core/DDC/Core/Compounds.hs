
module DDC.Core.Compounds 
        ( -- * Lets
          bindsOfLets
        , specBindsOfLets
        , valwitBindsOfLets

          -- * Patterns
        , bindsOfPat

          -- * Lambdas
        , makeXLAMs, makeXLams
        , takeXLAMs, takeXLams

          -- * Applications
        , makeXApps
        , takeXApps
        , takeXConApps
        , takeXPrimApps

          -- * Alternatives
        , takeCtorNameOfAlt)
where
import DDC.Type.Compounds
import DDC.Core.Exp


-- | Take the binds of a `Lets`
bindsOfLets :: Lets a n -> [Bind n]
bindsOfLets ll
 = case ll of
        LLet _ b _        -> [b]
        LRec bxs          -> map fst bxs
        LLetRegion   b bs -> b : bs
        LWithRegion{}     -> []


-- | Like `bindsOfLets` but only take the type binders.
specBindsOfLets :: Lets a n -> [Bind n]
specBindsOfLets ll
 = case ll of
        LLet _ _ _       -> []
        LRec _           -> []
        LLetRegion b _   -> [b]
        LWithRegion{}    -> []


-- | Like `bindsOfLets` but only take the value and witness binders.
valwitBindsOfLets :: Lets a n -> [Bind n]
valwitBindsOfLets ll
 = case ll of
        LLet _ b _       -> [b]
        LRec bxs         -> map fst bxs
        LLetRegion _ bs  -> bs
        LWithRegion{}    -> []


-- | Take the binds of a `Pat`
bindsOfPat :: Pat n -> [Bind n]
bindsOfPat pp
 = case pp of
        PDefault          -> []
        PData _ bs        -> bs


-- Lambdas ---------------------------------------------------------------------
-- | Make some nested lambda abstractions.
makeXLAMs :: a -> [Bind n] -> Exp a n -> Exp a n
makeXLAMs a bs x
        = foldr (XLAM a) x (reverse bs)


-- | Split nested lambdas from the front of an expression
--   or `Nothing` if there was no outer lambda
takeXLAMs :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLAMs xx
 = let  go bs (XLAM _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested lambda abstractions.
makeXLams :: a -> [Bind n] -> Exp a n -> Exp a n
makeXLams a bs x
        = foldr (XLam a) x (reverse bs)


-- | Split nested lambdas from the front of an expression
--   or `Nothing` if there was no outer lambda
takeXLams :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLams xx
 = let  go bs (XLam _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Applications ---------------------------------------------------------------
-- | Build sequence of type applications.
makeXApps   :: a -> Exp a n -> [Exp a n] -> Exp a n
makeXApps a t1 ts     = foldl (XApp a) t1 ts


-- | Flatten an application into the function parts and arguments, if any.
takeXApps   :: Exp a n -> [Exp a n]
takeXApps xx
 = case xx of
        XApp _ x1 x2    -> takeXApps x1 ++ [x2]
        _               -> [xx]


-- | Flatten an application of a primitive variable into the variable
--   and its arguments.
--   
--   Returns `Nothing` if the expression isn't a primop application.
takeXPrimApps :: Exp a n -> Maybe (n, [Exp a n])
takeXPrimApps xx
 = case takeXApps xx of
        XVar _ (UPrim p _) : xs  -> Just (p, xs)
        _                        -> Nothing

-- | Flatten an application of a data constructor into the constructor
--   and its arguments. 
--
--   Returns `Nothing` if the expression isn't a constructor application.
takeXConApps :: Exp a n -> Maybe (Bound n, [Exp a n])
takeXConApps xx
 = case takeXApps xx of
        XCon _ u : xs   -> Just (u, xs)
        _               -> Nothing


-- Alternatives ---------------------------------------------------------------
-- | Take the constructor name of an alternative, if there is one.
takeCtorNameOfAlt :: Alt a n -> Maybe n
takeCtorNameOfAlt aa
 = case aa of
        AAlt (PData u _) _      -> takeNameOfBound u
        _                       -> Nothing



