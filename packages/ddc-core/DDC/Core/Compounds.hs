
module DDC.Core.Compounds 
        ( bindsOfLets
        , takeXLams
        , takeXApps
        , takeXConApps
        , takeXPrimApps)
where
import DDC.Core.Exp


-- | Take the binds of a `Lets`
bindsOfLets :: Lets a n -> [Bind n]
bindsOfLets ll
 = case ll of
        LLet b _          -> [b]
        LRec bxs          -> map fst bxs
        LLetRegion   b bs -> b : bs
        LWithRegion{}     -> []


-- | Split nested lambdas from the front of an expression
--   or `Nothing` if there was no outer lambda
takeXLams :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLams xx
 = let  go bs (XLam _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


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

