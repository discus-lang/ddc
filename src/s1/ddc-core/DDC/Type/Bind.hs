
module DDC.Type.Bind
        (getBindType)
where
import DDC.Type.Exp


-- | Lookup the type of a bound thing from the binder stack.
--   The binder stack contains the binders of all the `TForall`s we've
--   entered under so far.
getBindType :: Eq n => [Bind n] -> Bound n -> Maybe (Int, Type n)
getBindType bs' u'
 = go 0 u' bs'
 where  go n u (BName n1 t : bs)
         | UName n2     <- u
         , n1 == n2     = Just (n, t)

         | otherwise    = go (n + 1) u bs

        go n (UIx i)    (BAnon t   : bs)
         | i < 0        = Nothing
         | i == 0       = Just (n, t)
         | otherwise    = go (n + 1) (UIx (i - 1)) bs

        go n u          (BAnon _   : bs)
         | otherwise    = go (n + 1) u bs

        go n u (BNone _   : bs)
         = go (n + 1) u bs

        go _ _ []       = Nothing
