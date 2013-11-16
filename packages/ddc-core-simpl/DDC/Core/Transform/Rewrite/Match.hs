-- | Create substitution to make (subst template) == target
module DDC.Core.Transform.Rewrite.Match
        ( -- * Substitutions
          SubstInfo
        , emptySubstInfo 

          -- * Matching
        , match)
where
import DDC.Core.Exp
import DDC.Type.Transform.Crush
import Data.Set                                 (Set)
import Data.Map                                 (Map)
import qualified DDC.Type.Sum                   as Sum
import qualified DDC.Type.Transform.AnonymizeT  as T
import qualified DDC.Core.Transform.AnonymizeX  as T
import qualified DDC.Core.Transform.Reannotate  as T
import qualified DDC.Type.Equiv                 as TE
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set


-------------------------------------------------------------------------------
-- | Value and type substition.
type SubstInfo a n 
        = (Map n (Exp a n), Map n (Type n))

-- | An empty substition info.
emptySubstInfo :: SubstInfo a n
emptySubstInfo 
        = (Map.empty, Map.empty)

lookupx n (xs,_) 
        = Map.lookup n xs

insertx n x (xs,tys) 
        = (Map.insert n x xs, tys)


-- Match Exp ------------------------------------------------------------------
-- | Create substitution to make (subst template) == target
--   Does not handle higher-order templates (ie ones with binders)
--
--  @ match emptySubstInfo (Set.fromList [r1, r2, s])
--   (stream [r1]  (unstream [r2]  s))
--   (stream [R0#] (unstream [R1#] (someStream 23))
--
--   => { r1 |-> R0#, r2 |-> R1, s |-> someStream 23 }
--  @
match   :: (Show a, Show n, Ord n)
        => SubstInfo a n        -- ^ Current substitution
        -> Set n                -- ^ Variables we're interested in
        -> Exp a n              -- ^ Template expression.
        -> Exp a n              -- ^ Target expression.
        -> Maybe (SubstInfo a n)

-- Variables bound by the rule: restricted to just UName earlier.
match m bs (XVar _ (UName n)) r
 | n `Set.member` bs
 -- Check if it's already been matched
 = case lookupx n m of
   Nothing -> return $ insertx n r m
   Just x  
    ->  -- Check if they're equal. Anonymize so names don't matter.
        -- Reannotate so annotations are ignored.
        let  x' = T.anonymizeX $ T.reannotate (const ()) x
             r' = T.anonymizeX $ T.reannotate (const ()) r
        in if   x' == r'
           then Just m
           else Nothing

match m _ (XVar _ v1) (XVar _ v2)
 | v1 == v2      = Just m

match m _ (XCon _ c1) (XCon _ c2)
 | c1 == c2      = Just m

match m bs (XApp _ x11 x12) (XApp _ x21 x22)
 = do   m' <- match m bs x11 x21
        match m' bs x12 x22

match m bs (XCast _ c1 x1) (XCast _ c2 x2)
 | eqCast c1 c2 
 = match m bs x1 x2

match (xs, tys) bs (XType _ t1) (XType _ t2)
 = do   tys' <- matchT t1 t2 bs tys
        return (xs, tys')

match m _ (XWitness _ w1) (XWitness _ w2)
 | eqWit w1 w2  = return m

match _ _ _ _ 
 = Nothing


eqCast :: Ord n => Cast a n -> Cast a n -> Bool
eqCast lc rc 
 = clean lc == clean rc
 where  clean c 
         = T.reannotate (const ())
         $ case c of
                CastWeakenEffect  eff   -> CastWeakenEffect  $ T.anonymizeT eff
                CastWeakenClosure clo   -> CastWeakenClosure $ map T.anonymizeX clo
                CastPurify        wit   -> CastPurify        wit
                CastForget        wit   -> CastForget wit
                CastBox                 -> CastBox
                CastRun                 -> CastRun


eqWit  :: Ord n => Witness a n -> Witness a n -> Bool
eqWit lw rw 
        =  T.reannotate (const ()) lw 
        == T.reannotate (const ()) rw


-- Types ----------------------------------------------------------------------
type VarSet n = Set.Set n
type Subst n  = Map.Map n (Type n)


-- | Try to find a simple substitution between two types.
--   Ignoring complicated effect sums.
--   Also ignoring TForall - checkRewriteRule outlaws foralls in the template, so it's safe.
--   Eg given template @a -> b@ and target @Int -> Float@,
--   returns substitution:
--      @{ a |-> Int, b |-> Float }@
--
matchT  :: Ord n
        => Type n       -- ^ Template type.
        -> Type n       -- ^ Target type.
        -> VarSet n     -- ^ Only attempt to match these names.
        -> Subst n      -- ^ Already matched (or @Map.empty@)
        -> Maybe (Subst n)

matchT t1 t2 vs subst
 = let  t1'     = unpackSumT $ crushSomeT t1
        t2'     = unpackSumT $ crushSomeT t2
   in case (t1', t2') of
        -- Constructor names must be equal.
        --
        -- Will this still work when it's a TyConBound - basically same as TVar?
        (TCon tc1,        TCon tc2)
         | tc1 == tc2
         -> Just subst

        -- Decend into applications.
        (TApp t11 t12,    TApp t21 t22)
         -> matchT t11 t21 vs subst >>= matchT t12 t22 vs

        -- Sums are equivalent if all of their components are.
        --   Very simple matching, only consider equivalent if both have same
        --   length and in the same order.
        --
        -- > (Read + Write + a) `matchT` (Read + Write + Alloc)
        -- > =
        -- > Just { a |-> Alloc }
        -- but
        -- > (Read + a + Write) `matchT` (Read + Write + Alloc)
        -- > =
        -- > Nothing
        -- and
        -- > (Read + Write + Alloc + a) `matchT` (Read + Write + Alloc)
        -- > =
        -- > Nothing
        -- despite a valid substitution existing as
        -- > { a |-> !0 }
        --
        (TSum ts1,        TSum ts2)
         -> let ts1'      = Sum.toList ts1
                ts2'      = Sum.toList ts2

                go (l:ls) (r:rs) s = matchT l r vs s >>= go ls rs
                go _      _      s = Just s
            in  if   length ts1' /= length ts2'
                then Nothing
                else go ts1' ts2' subst


        -- If template is in variable set, push the target into substitution
        (TVar (UName n), _)
         | Set.member n vs
         , Nothing <- Map.lookup n subst
         -> Just $ Map.insert n t2' subst

         | Set.member n vs
         , Just t1'' <- Map.lookup n subst
         , TE.equivT t1'' t2'
         -> Just subst

        -- Both are variables but it's not a template variable,
        -- so it's only valid if they're equal.
        (TVar (UName n), TVar v2)
         | not $ Set.member n vs
         , UName n == v2
         -> Just subst

        (TVar (UIx i), TVar v2)
         | UIx i == v2
         -> Just subst

        (TVar (UPrim n t), TVar v2)
         | UPrim n t == v2
         -> Just subst

        -- Otherwise the two are different
        (_, _)  -> Nothing


-- | Unpack single element sums into plain types.
unpackSumT :: Type n -> Type n
unpackSumT (TSum ts)
        | [t]   <- Sum.toList ts = t
unpackSumT tt                    = tt

