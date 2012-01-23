
module DDC.Type.Equiv
        (equivT)
where
import DDC.Type.Exp
import DDC.Type.Transform.Crush
import DDC.Type.Transform.Trim
import DDC.Base.Pretty
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

         -- Variables aren't name equivalent, but they might be equivalent
         -- if we renamed them. See if the binding occurrence has the same type.
         | depth1 == depth2
         , Just t1a      <- getBindType stack1 u1
         , Just t2a      <- getBindType stack2 u2
         -> equivT' stack1 depth1 stack2 depth2 t1a t2a

        -- Constructor names must be equal.
        (TCon tc1,        TCon tc2)
         -> tc1 == tc2

        -- Push binders on the stack as we enter foralls.
        (TForall b11 t12, TForall b21 t22)
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
                TyConComp TcConDeepRead   -> crushT tt
                TyConComp TcConDeepWrite  -> crushT tt
                TyConComp TcConDeepAlloc  -> crushT tt
                TyConComp TcConDeepUse    -> trimClosure tt
                _                         -> tt

        _ -> tt


-- | Lookup the type of a bound thing from the binder stack.
--   The binder stack contains the binders of all the `TForall`s we've
--   entered under so far.
getBindType :: Eq n => [Bind n] -> Bound n -> Maybe (Type n)
getBindType (BName n1 t : bs) uu@(UName n2 _)
        | n1 == n2      = Just t
        | otherwise     = getBindType bs uu

getBindType (BAnon t : bs)   uu@(UIx i _)
        | i == 0        = Just t
        | i <  0        = Nothing
        | otherwise     = getBindType bs uu

getBindType (_ : bs)         uu@(UIx i _)
        | i == 0        = Nothing
        | otherwise     = getBindType bs uu

getBindType _ _
        = Nothing

