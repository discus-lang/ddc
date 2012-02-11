
module DDC.Type.Equiv
        (equivT)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Transform.Crush
import DDC.Type.Transform.Trim
import DDC.Base.Pretty
import Data.Maybe
import qualified DDC.Type.Sum   as Sum


-- | Check equivalence of types.
--
--   We check equivalence upto alpha-renaming, as well as  crushing of effects
--   and trimming of closures.
--  
--   * We return False if we find any free variables.
--
--   * We assume the types are well-kinded, so that the type annotations on
--     bound variables match the binders.
--
equivT  :: (Ord n, Pretty n) => Type n -> Type n -> Bool
equivT t1 t2
        = equivT' [] 0 [] 0 t1 t2


equivT' :: (Ord n, Pretty n)
        => [Bind n] -> Int
        -> [Bind n] -> Int
        -> Type n   -> Type n
        -> Bool

equivT' stack1 depth1 stack2 depth2 t1 t2
 = let  t1'     = unpackSum $ crushSome t1
        t2'     = unpackSum $ crushSome t2
   in case (t1', t2') of
        (TVar u1,         TVar u2)
         -- Bound variables are name-equivalent.
         | u1 == u2     -> True

         -- Variables aren't name equivalent, 
         -- but would be equivalent if we renamed them.
         | depth1 == depth2
         , Just (ix1, t1a)   <- getBindType stack1 u1
         , Just (ix2, t2a)   <- getBindType stack2 u2
         , ix1 == ix2
         -> equivT' stack1 depth1 stack2 depth2 t1a t2a

        -- Constructor names must be equal.
        (TCon tc1,        TCon tc2)
         -> tc1 == tc2

        -- Push binders on the stack as we enter foralls.
        (TForall b11 t12, TForall b21 t22)
         |  equivT  (typeOfBind b11) (typeOfBind b21)
         -> equivT' (b11 : stack1) (depth1 + 1) 
                    (b21 : stack2) (depth2 + 1) 
                    t12 t22

        -- Decend into applications.
        (TApp t11 t12,    TApp t21 t22)
         -> equivT' stack1 depth1 stack2 depth2 t11 t21
         && equivT' stack1 depth1 stack2 depth2 t12 t22
        
        -- Sums are equivalent if all of their components are.
        (TSum ts1,        TSum ts2)                             -- TODO: doesn't handle types in the spill list.
         -> let ts1'    = Sum.toList ts1
                ts2'    = Sum.toList ts2

            in  and     $ (length ts1' == length ts2')
                        :  zipWith (equivT' stack1 depth1 stack2 depth2) 
                                  ts1' ts2'

        (_, _)  -> False


-- | Unpack single element sums into plain types.
unpackSum :: Type n -> Type n
unpackSum (TSum ts)
        | [t]   <- Sum.toList ts = t
unpackSum tt                     = tt


-- | Crush compound effects and closure terms.
--   We check for a crushable term before calling crushT because that function
--   will recursively crush the components. 
--   As equivT is already recursive, we don't want a doubly-recursive function
--   that tries to re-crush the same non-crushable type over and over.
--
crushSome :: (Ord n, Pretty n) => Type n -> Type n
crushSome tt
 = case tt of
        (TApp (TCon tc) _)
         -> case tc of
                TyConSpec    TcConDeepRead   -> crushT tt
                TyConSpec    TcConDeepWrite  -> crushT tt
                TyConSpec    TcConDeepAlloc  -> crushT tt

                -- If a closure is miskinded then 'trimClosure' 
                -- can return Nothing, so we just leave the term untrimmed.
                TyConSpec    TcConDeepUse    -> fromMaybe tt (trimClosure tt)

                TyConWitness TwConDeepGlobal -> crushT tt
                _                            -> tt

        _ -> tt


-- | Lookup the type of a bound thing from the binder stack.
--   The binder stack contains the binders of all the `TForall`s we've
--   entered under so far.
getBindType :: Eq n => [Bind n] -> Bound n -> Maybe (Int, Type n)
getBindType bs' u
 = go 0 bs'
 where  go n (BName n1 t : bs)
         | UName n2 _   <- u
         , n1 == n2     = Just (n, t)
         | otherwise    = go (n + 1) bs


        go n (BAnon t   : bs)
         | UIx i _      <- u
         , i == 0       = Just (n, t)

         | UIx i _      <- u
         , i < 0        = Nothing

         | otherwise    = go (n + 1) bs


        go n (BNone _   : bs)
         = go (n + 1) bs

        go _ []         = Nothing

