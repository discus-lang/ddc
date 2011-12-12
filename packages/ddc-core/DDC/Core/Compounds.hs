
module DDC.Core.Compounds 
        ( bindsOfLets
        , takeXLams
        , takeXApps
        , takeXPrimApps)
where
import DDC.Core.Exp


-- | Take the binds of a `Lets`
bindsOfLets :: Lets a n -> [Bind n]
bindsOfLets ll
 = case ll of
        LLet b _        -> [b]
        LRec bxs        -> map fst bxs
        LRegion b bs    -> b : bs
        

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


-- | Flatten a primitive application into the primitive constructor, 
--   and its arguments, if any.
--   
--   Returns `Nothing` if the expression isn't a primitive or an applicatin of one.
takeXPrimApps :: Exp a n -> Maybe (n, [Exp a n])
takeXPrimApps xx
 = case takeXApps xx of
        XVar _ (UPrim p _) : xs  -> Just (p, xs)
        XCon _ (UPrim p _) : xs  -> Just (p, xs)
        _                        -> Nothing